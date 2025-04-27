import os
import sys
import subprocess
import uuid
import tempfile
import numpy as np
import pandas as pd
from flask import Flask, request, render_template, jsonify, abort, send_from_directory
from scipy import stats
import math

# --- Lifelines Import (NEW) ---
try:
    from lifelines import KaplanMeierFitter
    from lifelines.statistics import logrank_test

    LIFELINES_AVAILABLE = True
except ImportError:
    LIFELINES_AVAILABLE = False
    print("Warning: 'lifelines' library not found. Survival statistics endpoint will not work.")
    print("Install using: pip install lifelines")

# --- Configuration & Setup ---
app = Flask(__name__)
if not os.path.exists('static'): os.makedirs('static')
app.static_folder = 'static'
PDF_DIR = os.path.join(app.static_folder, 'temp_plots')
os.makedirs(PDF_DIR, exist_ok=True)

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
CORRELATION_R_SCRIPT_PATH = os.path.join(SCRIPT_DIR, 'correlation_plot.R')  # Corrected name
TWO_GROUP_R_SCRIPT_PATH = os.path.join(SCRIPT_DIR, 'two_group_plot.R')
SURVIVAL_R_SCRIPT_PATH = os.path.join(SCRIPT_DIR, 'survival_plot.R')  # New R script path
RSCRIPT_EXECUTABLE = 'Rscript'  # Adjust if needed

# Check R scripts exist (Optional checks for startup)
# ... (checks for correlation and two-group) ...
if not os.path.exists(SURVIVAL_R_SCRIPT_PATH):
    print(f"Warning: Survival R script not found at {SURVIVAL_R_SCRIPT_PATH}. Plotting will fail.")


# --- Helper Functions (Keep existing: calculate_correlation_stats, calculate_two_group_stats, run_r_script) ---
def calculate_correlation_stats(x_data, y_data):
    # ... (same as before) ...
    n = len(x_data)
    if n < 3: return "错误：相关性分析需要至少 3 对有效数据。"
    try:
        shapiro_x_stat, shapiro_x_p = stats.shapiro(x_data)
        shapiro_y_stat, shapiro_y_p = stats.shapiro(y_data)
        corr_r, corr_p = stats.pearsonr(x_data, y_data)
        if abs(corr_r) < 1.0:
            denominator = 1 - corr_r ** 2
            if denominator > 1e-10 and (n - 2) >= 0:
                corr_t = corr_r * math.sqrt((n - 2) / denominator)
            else:
                corr_t = float('nan')
        else:
            corr_t = float('inf') if corr_r != 0 else float('nan')

        normality_x_result = "正态" if shapiro_x_p > 0.05 else "非正态"
        normality_y_result = "正态" if shapiro_y_p > 0.05 else "非正态"
        corr_sig_level = 0.05
        corr_sig_text = f"< {corr_sig_level}" if corr_p < corr_sig_level else f">= {corr_sig_level}"
        corr_meaningful = "有" if corr_p < corr_sig_level else "无"
        corr_direction = ""
        if corr_p < corr_sig_level:
            if corr_r > 0:
                corr_direction = "，X与Y呈正相关"
            elif corr_r < 0:
                corr_direction = "，X与Y呈负相关"
            else:
                corr_direction = "，但相关系数为0"

        stats_text = f"""统计描述 (相关性分析)
样本量 n = {n}
第一列(X)正态性检验 P = {shapiro_x_p:.4f} ({normality_x_result})
第二列(Y)正态性检验 P = {shapiro_y_p:.4f} ({normality_y_result})
相关系数 r = {corr_r:.4f}，统计量 t = {corr_t:.4f}，P = {corr_p:.4f} ({corr_sig_text})
说明：相关系数{corr_meaningful}统计学意义{corr_direction}"""
        return stats_text
    except Exception as e:
        print(f"Error during correlation stats calculation: {e}")
        return f"相关性统计计算时发生错误: {e}"


def calculate_two_group_stats(group1_data, group2_data, group1_name="Group1", group2_name="Group2"):
    # ... (same as before) ...
    n1 = len(group1_data)
    n2 = len(group2_data)
    if n1 < 2 or n2 < 2: return f"错误：每组至少需要 2 个有效数据点进行比较 (组1: {n1}, 组2: {n2})。"
    try:
        shapiro1_stat, shapiro1_p = stats.shapiro(group1_data)
        shapiro2_stat, shapiro2_p = stats.shapiro(group2_data)
        normality1_result = "正态" if shapiro1_p > 0.05 else "非正态"
        normality2_result = "正态" if shapiro2_p > 0.05 else "非正态"
        levene_stat, levene_p = stats.levene(group1_data, group2_data)
        variance_equal = levene_p > 0.05
        variance_result = "方差齐" if variance_equal else "方差不齐"
        ttest_stat, ttest_p = stats.ttest_ind(group1_data, group2_data, equal_var=variance_equal,
                                              nan_policy='omit')  # omit NaNs just in case
        mean1, mean2 = np.mean(group1_data), np.mean(group2_data)
        sd1, sd2 = np.std(group1_data, ddof=1), np.std(group2_data, ddof=1)  # Sample SD
        t_sig_level = 0.05
        t_sig_text = f"< {t_sig_level}" if ttest_p < t_sig_level else f">= {t_sig_level}"
        comparison_result = ""
        if ttest_p < t_sig_level:
            comparison_result = f"，说明两组均值差异有统计学意义 ({group1_name} {'<' if mean1 < mean2 else '>'} {group2_name})"
        else:
            comparison_result = "，说明两组均值差异无统计学意义"
        stats_text = f"""统计描述 (两组比较: {group1_name} vs {group2_name})
组1样本量 N1 = {n1}，均值 M1 = {mean1:.3f}，标准差 SD1 = {sd1:.3f}
组2样本量 N2 = {n2}，均值 M2 = {mean2:.3f}，标准差 SD2 = {sd2:.3f}
---
组1正态性检验 P1 = {shapiro1_p:.4f} ({normality1_result})
组2正态性检验 P2 = {shapiro2_p:.4f} ({normality2_result})
方差齐性检验 (Levene) F = {levene_stat:.4f}，P = {levene_p:.4f} ({variance_result})
---
独立样本 T 检验 ({'Student' if variance_equal else 'Welch'}):
统计量 t = {ttest_stat:.4f}，P = {ttest_p:.4f} ({t_sig_text})
结论：P值 {t_sig_text}{comparison_result}"""
        return stats_text
    except Exception as e:
        print(f"Error during two-group stats calculation: {e}")
        return f"两组比较统计计算时发生错误: {e}"


def run_r_script(r_script_path, args_list):
    # ... (same as before) ...
    cmd = [RSCRIPT_EXECUTABLE, r_script_path] + args_list
    print(f"Running R command: {' '.join(cmd)}")
    try:
        process = subprocess.run(cmd, capture_output=True, text=True, check=False, encoding='utf-8')
        return process
    except FileNotFoundError:
        print(f"Error: '{RSCRIPT_EXECUTABLE}' not found. Is R installed and in PATH?")
        raise  # Re-raise the exception to be caught by the route
    except Exception as e:
        print(f"Error running subprocess {RSCRIPT_EXECUTABLE}: {e}")
        raise  # Re-raise


# --- NEW Helper: Survival Stats using Lifelines ---
def calculate_survival_stats(df):
    """Calculates median survival and log-rank test using lifelines."""
    if not LIFELINES_AVAILABLE:
        return "错误：'lifelines' 库未安装在服务器上，无法计算生存统计。"

    results = []
    kmf = KaplanMeierFitter()

    # Check for groups
    groups = df['Group'].unique()
    # Treat empty string group names as a single group if they are the only 'group'
    meaningful_groups = [g for g in groups if g and pd.notna(g)]  # Filter out empty/NaN groups

    if len(meaningful_groups) <= 1:
        # Single group analysis (or all groups were empty/NaN)
        group_name = meaningful_groups[0] if meaningful_groups else "Overall"
        T = df['Time']
        E = df['Status']
        kmf.fit(T, event_observed=E, label=group_name)
        median_survival = kmf.median_survival_time_
        results.append(f"分组: {group_name}")
        results.append(f"  样本量 N = {len(T)}")
        results.append(f"  事件数 Events = {int(E.sum())}")
        results.append(f"  生存时间中位数 Median Survival Time = {median_survival:.3f}" if pd.notna(
            median_survival) else "  生存时间中位数 Median Survival Time = 未达到 (Not Reached)")
        results.append("  (无 Log-rank 检验，仅单个分组)")
    else:
        # Multiple group analysis
        group_results = {}
        logrank_data = []  # Prepare data for logrank test: list of tuples (durations, events, label)

        results.append(f"分组比较 (Log-Rank Test):")
        for group_name in meaningful_groups:
            group_df = df[df['Group'] == group_name]
            T = group_df['Time']
            E = group_df['Status']
            kmf.fit(T, event_observed=E, label=group_name)
            median_survival = kmf.median_survival_time_

            group_results[group_name] = {
                "N": len(T),
                "Events": int(E.sum()),
                "Median": f"{median_survival:.3f}" if pd.notna(median_survival) else "未达到"
            }
            logrank_data.append((T, E))  # Add data for test

        # Perform log-rank test
        # lifelines logrank_test takes durations and event_observed for each group as separate arguments
        durations = [data[0] for data in logrank_data]
        event_observed = [data[1] for data in logrank_data]
        try:
            logrank_result = logrank_test(*durations, *event_observed, t_0=- 1)  # t_0=-1 uses all data
            p_value = logrank_result.p_value
            test_statistic = logrank_result.test_statistic
            results.append(f"  Log-rank 检验统计量 = {test_statistic:.4f}，P = {p_value:.4f}")
            if p_value < 0.05:
                results.append("  结论：不同分组间的生存曲线差异具有统计学意义 (P < 0.05)。")
            else:
                results.append("  结论：不同分组间的生存曲线差异无统计学意义 (P >= 0.05)。")
        except Exception as lr_err:
            results.append(f"  Log-rank 检验失败: {lr_err}")

        results.append("\n各组描述:")
        for group_name, stats in group_results.items():
            results.append(f"  分组: {group_name}")
            results.append(f"    样本量 N = {stats['N']}")
            results.append(f"    事件数 Events = {stats['Events']}")
            results.append(f"    生存时间中位数 = {stats['Median']}")

    return "\n".join(results)


# --- Flask Routes ---
@app.route('/')
def index(): return render_template('index.html')


@app.route('/plots/<filename>')
def serve_plot(filename): return send_from_directory(PDF_DIR, filename, mimetype='application/pdf')


# --- Correlation Endpoints ---
@app.route('/generate_stats', methods=['POST'])
def generate_correlation_stats():
    # ... (same as before) ...
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')
    if not table_data: abort(400, description="未找到 'table_data'")
    try:
        df = pd.DataFrame(table_data, columns=['X', 'Y']).astype(float)
        stats_text = calculate_correlation_stats(df['X'].values, df['Y'].values)
        return jsonify({"stats_text": stats_text})
    except (ValueError, KeyError, TypeError) as e:
        abort(400, description=f"数据格式错误或不足: {e}")
    except Exception as e:
        abort(500, description=f"生成相关性统计时发生内部错误: {e}")


@app.route('/generate_plot', methods=['POST'])
def generate_correlation_plot():
    # ... (same as before) ...
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')
    plot_method = req_data.get('plot_method', 'lm')
    if not table_data: abort(400, description="未找到 'table_data'")
    try:
        df = pd.DataFrame(table_data, columns=['X', 'Y']).astype(float)
        if len(df) < 3: abort(400, description="数据不足 (<3 行)")
    except Exception as e:
        abort(400, description=f"数据格式错误: {e}")

    tmp_csv_file = None
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix=".csv", delete=False, encoding='utf-8') as tmp_csv_file:
            df.to_csv(tmp_csv_file.name, index=False)
            input_csv_path = tmp_csv_file.name
        pdf_filename = f"Correlation_plot_{uuid.uuid4().hex}.pdf"
        output_pdf_path = os.path.join(PDF_DIR, pdf_filename)
        args = ['--input', input_csv_path, '--output', output_pdf_path, '--method', plot_method]
        process = run_r_script(CORRELATION_R_SCRIPT_PATH, args)
        if process.returncode != 0:
            error_msg = f"相关性 R 脚本执行失败: {process.stderr or process.stdout or 'Unknown R error'}"
            print(error_msg)
            abort(500, description=error_msg.splitlines()[0])
        if not os.path.exists(output_pdf_path): abort(500, description="R 脚本成功，但未找到输出 PDF。")
        return jsonify({"pdf_url": f"/plots/{pdf_filename}"})
    except Exception as e:
        abort(500, description=f"生成相关性图表时出错: {e}")
    finally:
        if tmp_csv_file and os.path.exists(tmp_csv_file.name): os.remove(tmp_csv_file.name)


# --- Two-Group Endpoints ---
@app.route('/generate_two_group_stats', methods=['POST'])
def generate_two_group_stats():
    # ... (same as before) ...
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')  # Expecting [[group_name, value], ...]
    if not table_data: abort(400, description="未找到 'table_data'")
    try:
        df = pd.DataFrame(table_data, columns=['Group', 'Value'])
        df['Value'] = df['Value'].astype(float)  # Ensure Value is numeric
        groups = df['Group'].unique()
        if len(groups) != 2: abort(400,
                                   description=f"需要恰好两个分组，但找到了 {len(groups)} 个: {', '.join(map(str, groups))}")
        group1_name, group2_name = groups[0], groups[1]
        group1_data = df[df['Group'] == group1_name]['Value'].values
        group2_data = df[df['Group'] == group2_name]['Value'].values
        if len(group1_data) < 2 or len(group2_data) < 2: abort(400,
                                                               description=f"每组至少需要 2 个数据点 (组 '{group1_name}': {len(group1_data)}, 组 '{group2_name}': {len(group2_data)})")
        stats_text = calculate_two_group_stats(group1_data, group2_data, str(group1_name), str(group2_name))
        return jsonify({"stats_text": stats_text})
    except (ValueError, KeyError, TypeError) as e:
        abort(400, description=f"数据格式错误或不足: {e}")
    except Exception as e:
        abort(500, description=f"生成两组比较统计时发生内部错误: {e}")


@app.route('/generate_two_group_plot', methods=['POST'])
def generate_two_group_plot():
    # ... (same as before) ...
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')  # Expecting [[group_name, value], ...]
    plot_method = req_data.get('plot_method', 'boxplot')  # Default plot type
    if not table_data: abort(400, description="未找到 'table_data'")
    try:
        df = pd.DataFrame(table_data, columns=['Group', 'Value'])
        df['Value'] = df['Value'].astype(float)
        groups = df['Group'].unique()
        if len(groups) != 2: abort(400, description=f"需要恰好两个分组，但找到了 {len(groups)} 个")
        if len(df[df['Group'] == groups[0]]) < 2 or len(df[df['Group'] == groups[1]]) < 2: abort(400,
                                                                                                 description="每组至少需要2个数据点")
    except Exception as e:
        abort(400, description=f"数据格式错误: {e}")

    tmp_csv_file = None
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix=".csv", delete=False, encoding='utf-8') as tmp_csv_file:
            df.to_csv(tmp_csv_file.name, index=False)
            input_csv_path = tmp_csv_file.name
        pdf_filename = f"TwoGroup_plot_{uuid.uuid4().hex}.pdf"
        output_pdf_path = os.path.join(PDF_DIR, pdf_filename)
        if not os.path.exists(TWO_GROUP_R_SCRIPT_PATH): abort(500,
                                                              description="Two-group R script not found on server.")
        args = ['--input', input_csv_path, '--output', output_pdf_path, '--plottype', plot_method]
        process = run_r_script(TWO_GROUP_R_SCRIPT_PATH, args)
        if process.returncode != 0:
            error_msg = f"两组比较 R 脚本执行失败: {process.stderr or process.stdout or 'Unknown R error'}"
            print(error_msg)
            abort(500, description=error_msg.splitlines()[0])
        if not os.path.exists(output_pdf_path): abort(500, description="R 脚本成功，但未找到输出 PDF。")
        return jsonify({"pdf_url": f"/plots/{pdf_filename}"})
    except Exception as e:
        abort(500, description=f"生成两组比较图表时出错: {e}")
    finally:
        if tmp_csv_file and os.path.exists(tmp_csv_file.name): os.remove(tmp_csv_file.name)


# --- NEW Survival Analysis Endpoints ---
@app.route('/generate_survival_stats', methods=['POST'])
def generate_survival_stats():
    if not LIFELINES_AVAILABLE: abort(501,
                                      description="Survival analysis feature not available: 'lifelines' library missing.")
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')  # Expecting [[time, status, group], ...]
    if not table_data: abort(400, description="未找到 'table_data'")

    try:
        df = pd.DataFrame(table_data, columns=['Time', 'Status', 'Group'])
        # Validate data types
        df['Time'] = pd.to_numeric(df['Time'])
        df['Status'] = pd.to_numeric(df['Status']).astype(int)  # Ensure integer 0 or 1
        df['Group'] = df['Group'].astype(str).fillna('')  # Treat NaN/None groups as empty string

        # Basic validation
        if df['Time'].min() < 0: abort(400, description="时间 (Time) 不能为负数。")
        if not df['Status'].isin([0, 1]).all(): abort(400, description="状态 (Status) 必须为 0 或 1。")
        if len(df) < 3: abort(400, description="生存分析至少需要 3 条有效数据。")

        stats_text = calculate_survival_stats(df)
        return jsonify({"stats_text": stats_text})

    except (ValueError, KeyError, TypeError) as e:
        abort(400, description=f"数据格式错误或不足: {e}")
    except Exception as e:
        abort(500, description=f"生成生存分析统计时发生内部错误: {e}")


@app.route('/generate_survival_plot', methods=['POST'])
def generate_survival_plot():
    if not request.is_json: abort(400, description="请求必须是 JSON 格式")
    req_data = request.get_json()
    table_data = req_data.get('table_data')
    plot_options = req_data.get('plot_options', {})  # Get options like {show_ci: true, show_risk_table: false}
    if not table_data: abort(400, description="未找到 'table_data'")

    try:
        df = pd.DataFrame(table_data, columns=['Time', 'Status', 'Group'])
        # Validate data types
        df['Time'] = pd.to_numeric(df['Time'])
        df['Status'] = pd.to_numeric(df['Status']).astype(int)
        df['Group'] = df['Group'].astype(str).fillna('')  # Use empty string for no group

        if df['Time'].min() < 0: abort(400, description="时间 (Time) 不能为负数。")
        if not df['Status'].isin([0, 1]).all(): abort(400, description="状态 (Status) 必须为 0 或 1。")
        if len(df) < 3: abort(400, description="生存分析至少需要 3 条有效数据。")

    except Exception as e:
        abort(400, description=f"数据格式错误: {e}")

    tmp_csv_file = None
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix=".csv", delete=False, encoding='utf-8') as tmp_csv_file:
            # R script expects 'Time', 'Status', 'Group' columns
            df.to_csv(tmp_csv_file.name, index=False)
            input_csv_path = tmp_csv_file.name

        pdf_filename = f"Survival_plot_{uuid.uuid4().hex}.pdf"
        output_pdf_path = os.path.join(PDF_DIR, pdf_filename)

        # Prepare arguments for R script based on plot_options
        args = ['--input', input_csv_path, '--output', output_pdf_path]
        if plot_options.get('show_ci'):
            args.extend(['--show_ci', 'TRUE'])
        else:
            args.extend(['--show_ci', 'FALSE'])  # Explicitly pass FALSE if unchecked
        if plot_options.get('show_risk_table'):
            args.extend(['--show_risk_table', 'TRUE'])
        else:
            args.extend(['--show_risk_table', 'FALSE'])

        # Ensure the specific R script exists before calling
        if not os.path.exists(SURVIVAL_R_SCRIPT_PATH):
            abort(500, description="Survival analysis R script not found on server.")

        process = run_r_script(SURVIVAL_R_SCRIPT_PATH, args)

        if process.returncode != 0:
            error_msg = f"生存分析 R 脚本执行失败: {process.stderr or process.stdout or 'Unknown R error'}"
            print(error_msg)
            abort(500, description=error_msg.splitlines()[0])
        if not os.path.exists(output_pdf_path):
            abort(500, description="R 脚本成功，但未找到输出 PDF。")

        return jsonify({"pdf_url": f"/plots/{pdf_filename}"})
    except Exception as e:
        abort(500, description=f"生成生存分析图表时出错: {e}")
    finally:
        if tmp_csv_file and os.path.exists(tmp_csv_file.name): os.remove(tmp_csv_file.name)


if __name__ == '__main__':
    app.run(debug=True, port=5000)
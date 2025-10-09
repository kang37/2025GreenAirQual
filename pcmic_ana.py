# 导入所需的库。
# %%
import pandas as pd
import os
import pyarrow.feather as feather
from matplotlib import pyplot as plt
from tigramite import data_processing as pp
from tigramite import plotting as tp
from tigramite.pcmci import PCMCI
from tigramite.independence_tests import parcorr
import numpy as np


# 读取经过处理的原始数据。
# %%
# Bug：应该输出所有原始数据，然后读取后去除不用的列。
mar_dv = pd.read_csv("df_segments.csv")

# 函数：获取所需数据。
def get_data(mar_dv, var_names): 
    #if group_value == "Overall": 
    #    data_out = mar_dv
    #else: 
    #    data_out = mar_dv.loc[mar_dv]
    data_out = mar_dv

    data_out = data_out[var_names].values

    # print(f"{group_col} = {group_value}")
    print("Dim of data with NaN:", data_out.shape)
    
    nan_mask = np.isnan(data_out)
    rows_with_nan = np.any(nan_mask, axis=1)
    clean_data = data_out[~rows_with_nan]
    print("Dim of data without NaN:", clean_data.shape)
    
    return clean_data

# 基于先验知识构造可能的因果关系连接。
# Bug：目前假设所有变量之间均有连接，且连接不确定。
# 如果要测试时间滞后，则设置为tau in range(2)。
# link_assumptions = {
#     j:{(i, -tau):'o?o' 
#        for i in range(3) for tau in [0]} for j in range(3)
# }

# 定义函数
# %%
# 函数：
def my_pcmci(mar_dv, var_names, tau_min, pc_alpha, verbosity, period_id):
    data = get_data(mar_dv=mar_dv, var_names=var_names)

    dataframe = pp.DataFrame(data, var_names=var_names, missing_flag=999.)
    ParCorr = parcorr.ParCorr()
    pcmci = PCMCI(dataframe=dataframe, cond_ind_test=ParCorr, verbosity=verbosity)

    results = pcmci.run_pcmciplus(
        tau_min=tau_min, tau_max=3, pc_alpha=pc_alpha
    )

    plt.figure()
    tp.plot_graph(
        arrow_linewidth=3.0,
        figsize=(12*0.4, 5*0.4),
        vmin_edges=-0.5,
        vmax_edges=0.5,
        node_label_size=5,
        node_size=0.3,
        link_label_fontsize=5,
        val_matrix=results['val_matrix'],
        graph=results['graph'],
        var_names=var_names,
        link_colorbar_label='cross-MCI (edges)',
        node_colorbar_label='auto-MCI (nodes)',
        label_fontsize=5,
        show_colorbar=0
    );
    plt.savefig(f"data_proc/{period_id}.png", dpi=800)
    plt.close()
    
    return results

pc_alpha = 0.05
var_names = [
    "o3_8h",
    "tavg",
    "rh", 
    "precip",
    "ndvi"
]

# 循环分析
# %%
for res_stat_id_unique in mar_dv['res_stat_id'].unique(): 
    my_pcmci(
            mar_dv=mar_dv[mar_dv['res_stat_id'] == res_stat_id_unique],
            var_names=var_names,
            tau_min=1,
            pc_alpha=pc_alpha,
            verbosity=0, 
            period_id=res_stat_id_unique
        )


# %%

group_columns = {
    "geo_region": ['华东', '西南', '华北', '东北', '西北', '中南'],
    "econ_region": mar_dv["econ_region"].unique(),
    "totgdp_class": mar_dv["totgdp_class"].unique(),
    "pcgdp_class": mar_dv["pcgdp_class"].unique(),
    "urrate_class": mar_dv["urrate_class"].unique()
}

# 分组分析。
for group_col, group_values in group_columns.items():
    for value in group_values:
        my_pcmci(
            mar_dv=mar_dv,
            group_col=group_col,
            group_value=value,
            var_names=var_names,
            tau_min=0,
            pc_alpha=pc_alpha,
            verbosity=0
        )

# 不分组分析。
my_pcmci(
    mar_dv=mar_dv,
    group_col="Overall",
    group_value="Overall",
    var_names=var_names,
    tau_min=0,
    pc_alpha=pc_alpha,
    verbosity=0
)
# %%

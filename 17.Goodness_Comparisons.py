import matplotlib.pyplot as plt
import os
import pandas as pd
import feather

#scale = 1.0 # for presentation
scale = 1.5 # for paper
scale = 1.4 # for paper


# load
df_LP = feather.read_dataframe(os.path.expanduser("./Orbis_LP_testresults.feather"))
df_LP_Change = feather.read_dataframe(os.path.expanduser("./Orbis_LP_Change_testresults.feather"))

# compute differences
df_LP["Likelihood_Levy_minus_AEP"] = df_LP["Levy_CV_Likelihood"] - df_LP["AEP_CV_Likelihood"]
df_LP_Change["Likelihood_Levy_minus_AEP"] = df_LP_Change["Levy_CV_Likelihood"] - df_LP_Change["AEP_CV_Likelihood"]
df_LP["AIC_Levy_minus_AEP"] = df_LP["Levy_AIC"] - df_LP["AEP_AIC"]
df_LP_Change["AIC_Levy_minus_AEP"] = df_LP_Change["Levy_AIC"] - df_LP_Change["AEP_AIC"]


colors = ["royalblue", "deepskyblue"]

fig, ax = plt.subplots(1, 3, figsize=(scale*7.5, scale*2.5))
for i, df in enumerate([df_LP, df_LP_Change]):
    xmin = min(list(df["AEP_CV_Likelihood"]/1000000.))
    xmax = max(list(df["AEP_CV_Likelihood"]/1000000.))
    ymin = min(list(df["Likelihood_Levy_minus_AEP"]/1000.))
    ymax = max(list(df["Likelihood_Levy_minus_AEP"]/1000.))
    ax[0].fill_between([xmin*1.1,xmax*0.9], [min(-2000/1000., ymin*1.1), min(-2000/1000., ymin*1.1)], [0,0], facecolor="#880022", alpha=0.1)
    ax[0].plot([xmin*1.1,xmax*0.9], [0,0], color="k")
    ax[0].scatter(list(df["AEP_CV_Likelihood"]/1000000.), list(df["Likelihood_Levy_minus_AEP"]/1000.), color=colors[i], s=2)
    ax[0].set_title("k-fold cross-validation")
    ax[0].set_ylabel("L(Levy)-L(AEP) in '000")
    ax[0].set_xlabel("L(AEP) in million")
    ax[0].set_xlim(xmin*1.1, xmax*0.9)
    ax[0].set_ylim(min(-2000/1000., ymin*1.1), max(ymax*1.1, 50.))
    ax[0].xaxis.set_major_locator(plt.MaxNLocator(nbins=3, min_n_ticks=3))
handles = []
for i, df in enumerate([df_LP, df_LP_Change]):
    xymin = min(list(df["AEP_Soofi_ID_S"]) + list(df["Levy_Soofi_ID_S"]))
    xymax = max(list(df["AEP_Soofi_ID_S"]) + list(df["Levy_Soofi_ID_S"]))
    ax[1].fill_between([xymin*0.8, xymax*1.01], [xymin*.8, xymin*.8], [xymin*0.8, xymax*1.01], facecolor="#880022", alpha=0.1)
    ax[1].plot([xymin*0.9, xymax*1.1], [xymin*0.9, xymax*1.1], color="k")
    h = ax[1].scatter(list(df["AEP_Soofi_ID_S"]), list(df["Levy_Soofi_ID_S"]), color=colors[i], s=2)
    handles.append(h)
    ax[1].set_title("Soofi ID score")
    ax[1].set_ylabel("Soofi ID score Levy")
    ax[1].set_xlabel("Soofi ID score AEP")
    ax[1].set_xlim(xymin*0.9,xymax*1.01)
    ax[1].set_ylim(xymin*0.9,xymax*1.01)
    ax[1].legend(handles, ('LP', 'LP Change'), loc=3, markerscale=2)
for i, df in enumerate([df_LP, df_LP_Change]):
    xmin = min(list(df["AEP_AIC"]/1000000.))
    xmax = max(list(df["AEP_AIC"]/1000000.))
    ymin = min(list(df["AIC_Levy_minus_AEP"]/1000.))
    ymax = max(list(df["AIC_Levy_minus_AEP"]/1000.))
    ax[2].fill_between([xmin*1.1,xmax*0.9], [0, 0], [max(10000/1000., ymax*1.1), max(10000/1000., ymax*1.1)], facecolor="#880022", alpha=0.1)
    ax[2].plot([xmin*1.1,xmax*0.9], [0,0], color="k")
    ax[2].scatter(list(df["AEP_AIC"]/1000000.), list(df["AIC_Levy_minus_AEP"]/1000.), color=colors[i], s=2)
    ax[2].set_title("AIC")
    ax[2].set_ylabel("AIC(Levy)-AIC(AEP) in '000")
    ax[2].set_xlabel("AIC(AEP) in million")
    ax[2].set_xlim(xmin*1.1, xmax*0.9)
    ax[2].set_ylim(min(-1000/1000., ymin*1.1), max(10000/1000., ymax*1.1))
    ax[2].xaxis.set_major_locator(plt.MaxNLocator(nbins=3, min_n_ticks=3))

   
plt.tight_layout()
plt.savefig(os.path.expanduser("~/tmp/goodness_comparisons_2_3_fixedlabels.pdf"))
#plt.show()

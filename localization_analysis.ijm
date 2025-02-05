dir1 = getDirectory("");
dir2 = getDirectory("");
dir3 = getDirectory("");
list = getFileList(dir1);
setBatchMode(true);
for (i=0; i<list.length; i++) {
    showProgress(i+1, list.length);
    open(dir1+list[i]);
suf1= ".json";
suf2=".png";

run("Run analysis", "filter=[Wavelet filter (B-Spline)] scale=2.0 order=3 detector=[Local maximum] connectivity=8-neighbourhood threshold=std(Wave.F1) estimator=[PSF: Integrated Gaussian] sigma=1.6 method=[Weighted Least squares] full_image_fitting=false fitradius=3 mfaenabled=false renderer=[Averaged shifted histograms] magnification=5.0 colorizez=false shifts=2 repaint=50 threed=false");
run("Export results", "filepath=["+dir2+list[i]+suf1+"] fileformat=JSON id=true frame=true sigma=true chi2=true bkgstd=true intensity=true saveprotocol=true offset=true uncertainty=true y=true x=true");
run("Close");
saveAs("tiff", dir3+list[i]);

run("In [+]");
run("In [+]");
run("Out [-]");
run("Out [-]");
run("Close");
run("Close");
}
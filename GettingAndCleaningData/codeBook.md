##Getting and Cleaning Data Course Project

###Source of the data
Human Activity Recognition Using Smartphones Dataset
Version 1.0

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy. activityrecognition@smartlab.ws www.smartlab.ws

###Data set information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

###For each record it is provided:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

###Script used to create codeBook.rm

```R
make.codeBook <- function(set) {
      df <- data.frame(
            row.names = NULL,
            column.names = names(set),
            class = sapply(set, class),
            range = sapply(set, function(x)
                  if (class(x) == "factor")
                        paste(levels(x), collapse = " / ")
                  else if (class(x) == "numeric" ||
                           class(x) == "integer")
                        paste(min(x), max(x), sep = "  /  ")
                  else
                        class(x)),
            mean = sapply(set, function(x)
                  if (class(x) == "numeric")
                        mean(x)
                  else
                        "Not available")
      )
      write.table(df, "codeBook.md", sep = " | ")
}
```

###Variables information
"row.number" | "column.names" | "class" | "range" | "mean"
--- | --- | --- | --- | ---
"1" | "activity" | "factor" | "LAYING / SITTING / STANDING / WALKING / WALKING_DOWNSTAIRS / WALKING_UPSTAIRS" | "Not available"
"2" | "subject" | "numeric" | "1 / 30" | "Not available"
"3" | "Time Body accelerometer mean()-X_Mean" | "numeric" | "0.22159824394  /  0.3014610196" | "0.274302742245795"
"4" | "Time Body accelerometer mean()-Y_Mean" | "numeric" | "-0.0405139534294  /  -0.00130828765170213" | "-0.0178755238674415"
"5" | "Time Body accelerometer mean()-Z_Mean" | "numeric" | "-0.152513899520833  /  -0.07537846886" | "-0.109163815804519"
"6" | "Time Body accelerometer std()-X_Mean" | "numeric" | "-0.996068635384615  /  0.626917070512821" | "-0.557690076404401"
"7" | "Time Body accelerometer std()-Y_Mean" | "numeric" | "-0.990240946666667  /  0.616937015333333" | "-0.460462635378301"
"8" | "Time Body accelerometer std()-Z_Mean" | "numeric" | "-0.987658662307692  /  0.609017879074074" | "-0.575560246148636"
"9" | "Time Gravity accelerometer mean()-X_Mean" | "numeric" | "-0.680043155060241  /  0.974508732" | "0.697477505882702"
"10" | "Time Gravity accelerometer mean()-Y_Mean" | "numeric" | "-0.479894842941176  /  0.956593814210526" | "-0.0162128361521394"
"11" | "Time Gravity accelerometer mean()-Z_Mean" | "numeric" | "-0.49508872037037  /  0.9578730416" | "0.0741278709325255"
"12" | "Time Gravity accelerometer std()-X_Mean" | "numeric" | "-0.996764227384615  /  -0.829554947808219" | "-0.96375253077172"
"13" | "Time Gravity accelerometer std()-Y_Mean" | "numeric" | "-0.99424764884058  /  -0.643578361424658" | "-0.952429559765945"
"14" | "Time Gravity accelerometer std()-Z_Mean" | "numeric" | "-0.990957249538462  /  -0.610161166287671" | "-0.93640104156585"
"15" | "Time Body accelerometer Jerk mean()-X_Mean" | "numeric" | "0.0426880986186441  /  0.130193043809524" | "0.0794735599203562"
"16" | "Time Body accelerometer Jerk mean()-Y_Mean" | "numeric" | "-0.0386872111282051  /  0.056818586275" | "0.00756520996888408"
"17" | "Time Body accelerometer Jerk mean()-Z_Mean" | "numeric" | "-0.0674583919268293  /  0.0380533591627451" | "-0.00495340328183431"
"18" | "Time Body accelerometer Jerk std()-X_Mean" | "numeric" | "-0.994604542264151  /  0.544273037307692" | "-0.594946699510964"
"19" | "Time Body accelerometer Jerk std()-Y_Mean" | "numeric" | "-0.989513565652174  /  0.355306716915385" | "-0.565414714340423"
"20" | "Time Body accelerometer Jerk std()-Z_Mean" | "numeric" | "-0.993288313333333  /  0.0310157077775926" | "-0.735957689241115"
"21" | "Time Body gyroscope mean()-X_Mean" | "numeric" | "-0.205775427307692  /  0.19270447595122" | "-0.0324371599031218"
"22" | "Time Body gyroscope mean()-Y_Mean" | "numeric" | "-0.204205356087805  /  0.0274707556666667" | "-0.0742595723452297"
"23" | "Time Body gyroscope mean()-Z_Mean" | "numeric" | "-0.0724546025804878  /  0.179102058245614" | "0.0874446468695526"
"24" | "Time Body gyroscope std()-X_Mean" | "numeric" | "-0.994276591304348  /  0.267657219333333" | "-0.691639902777431"
"25" | "Time Body gyroscope std()-Y_Mean" | "numeric" | "-0.994210471914894  /  0.476518714444444" | "-0.653302029911363"
"26" | "Time Body gyroscope std()-Z_Mean" | "numeric" | "-0.985538363333333  /  0.564875818162963" | "-0.616435294332593"
"27" | "Time Body gyroscope Jerk mean()-X_Mean" | "numeric" | "-0.157212539189362  /  -0.0220916265065217" | "-0.0960567959204382"
"28" | "Time Body gyroscope Jerk mean()-Y_Mean" | "numeric" | "-0.0768089915604167  /  -0.0132022768074468" | "-0.0426927819752453"
"29" | "Time Body gyroscope Jerk mean()-Z_Mean" | "numeric" | "-0.0924998531372549  /  -0.00694066389361702" | "-0.0548018825799509"
"30" | "Time Body gyroscope Jerk std()-X_Mean" | "numeric" | "-0.99654254057971  /  0.179148649684615" | "-0.703632714557601"
"31" | "Time Body gyroscope Jerk std()-Y_Mean" | "numeric" | "-0.997081575652174  /  0.295945926186441" | "-0.763551835158898"
"32" | "Time Body gyroscope Jerk std()-Z_Mean" | "numeric" | "-0.995380794637681  /  0.193206498960417" | "-0.709559184010004"
"33" | "Time Body accelerometer Magnitude mean()_Mean" | "numeric" | "-0.986493196666667  /  0.644604325128205" | "-0.49728966685894"
"34" | "Time Body accelerometer Magnitude std()_Mean" | "numeric" | "-0.986464542615385  /  0.428405922622222" | "-0.543908670845839"
"35" | "Time Gravity accelerometer Magnitude mean()_Mean" | "numeric" | "-0.986493196666667  /  0.644604325128205" | "-0.49728966685894"
"36" | "Time Gravity accelerometer Magnitude std()_Mean" | "numeric" | "-0.986464542615385  /  0.428405922622222" | "-0.543908670845839"
"37" | "Time Body accelerometer Jerk Magnitude mean()_Mean" | "numeric" | "-0.99281471515625  /  0.434490400974359" | "-0.607929591545179"
"38" | "Time Body accelerometer Jerk Magnitude std()_Mean" | "numeric" | "-0.994646916811594  /  0.450612065720513" | "-0.584175609709768"
"39" | "Time Body gyroscope Magnitude mean()_Mean" | "numeric" | "-0.980740846769231  /  0.418004608615385" | "-0.565163077212988"
"40" | "Time Body gyroscope Magnitude std()_Mean" | "numeric" | "-0.981372675614035  /  0.299975979851852" | "-0.630394720315622"
"41" | "Time Body gyroscope Jerk Magnitude mean()_Mean" | "numeric" | "-0.997322526811594  /  0.0875816618205128" | "-0.736369300428253"
"42" | "Time Body gyroscope Jerk Magnitude std()_Mean" | "numeric" | "-0.997666071594203  /  0.250173204117966" | "-0.755015188509002"
"43" | "Fast Fourier Body accelerometer mean()-X_Mean" | "numeric" | "-0.995249932641509  /  0.537012022051282" | "-0.575799983503946"
"44" | "Fast Fourier Body accelerometer mean()-Y_Mean" | "numeric" | "-0.989034304057971  /  0.524187686888889" | "-0.488732713013952"
"45" | "Fast Fourier Body accelerometer mean()-Z_Mean" | "numeric" | "-0.989473926666667  /  0.280735952206667" | "-0.62973875362598"
"46" | "Fast Fourier Body accelerometer std()-X_Mean" | "numeric" | "-0.996604570307692  /  0.658506543333333" | "-0.552201112392524"
"47" | "Fast Fourier Body accelerometer std()-Y_Mean" | "numeric" | "-0.990680395362319  /  0.560191344" | "-0.481478729871355"
"48" | "Fast Fourier Body accelerometer std()-Z_Mean" | "numeric" | "-0.987224804307692  /  0.687124163703704" | "-0.582361415029381"
"49" | "Fast Fourier Body accelerometer meanFreq()-X_Mean" | "numeric" | "-0.635913046346154  /  0.159123629063636" | "-0.23226609715376"
"50" | "Fast Fourier Body accelerometer meanFreq()-Y_Mean" | "numeric" | "-0.379518455061538  /  0.466528231788462" | "0.0115288797872382"
"51" | "Fast Fourier Body accelerometer meanFreq()-Z_Mean" | "numeric" | "-0.520114793584906  /  0.402532553395833" | "0.0437174260645842"
"52" | "Fast Fourier Body accelerometer Jerk mean()-X_Mean" | "numeric" | "-0.994630797358491  /  0.474317256051282" | "-0.613928222283428"
"53" | "Fast Fourier Body accelerometer Jerk mean()-Y_Mean" | "numeric" | "-0.989398823913043  /  0.276716853307692" | "-0.588163069360073"
"54" | "Fast Fourier Body accelerometer Jerk mean()-Z_Mean" | "numeric" | "-0.992018447826087  /  0.157775692377778" | "-0.714358487490646"
"55" | "Fast Fourier Body accelerometer Jerk std()-X_Mean" | "numeric" | "-0.995073759245283  /  0.476803887476923" | "-0.612103283207987"
"56" | "Fast Fourier Body accelerometer Jerk std()-Y_Mean" | "numeric" | "-0.990468082753623  /  0.349771285415897" | "-0.570730968650136"
"57" | "Fast Fourier Body accelerometer Jerk std()-Z_Mean" | "numeric" | "-0.993107759855072  /  -0.00623647528983051" | "-0.756489426411787"
"58" | "Fast Fourier Body accelerometer Jerk meanFreq()-X_Mean" | "numeric" | "-0.576044001875  /  0.331449281481482" | "-0.0691017912141093"
"59" | "Fast Fourier Body accelerometer Jerk meanFreq()-Y_Mean" | "numeric" | "-0.601971415384615  /  0.195677336307692" | "-0.228102065671109"
"60" | "Fast Fourier Body accelerometer Jerk meanFreq()-Z_Mean" | "numeric" | "-0.62755547372549  /  0.230107945944444" | "-0.137602308791712"
"61" | "Fast Fourier Body gyroscope mean()-X_Mean" | "numeric" | "-0.99312260884058  /  0.474962448333333" | "-0.636739605053057"
"62" | "Fast Fourier Body gyroscope mean()-Y_Mean" | "numeric" | "-0.994025488297872  /  0.328817010088889" | "-0.676686800745998"
"63" | "Fast Fourier Body gyroscope mean()-Z_Mean" | "numeric" | "-0.985957788  /  0.492414379822222" | "-0.604391244378742"
"64" | "Fast Fourier Body gyroscope std()-X_Mean" | "numeric" | "-0.994652185217391  /  0.196613286661538" | "-0.711035658050846"
"65" | "Fast Fourier Body gyroscope std()-Y_Mean" | "numeric" | "-0.994353086595745  /  0.646233637037037" | "-0.645433416234092"
"66" | "Fast Fourier Body gyroscope std()-Z_Mean" | "numeric" | "-0.986725274871795  /  0.522454216314815" | "-0.657746585870822"
"67" | "Fast Fourier Body gyroscope meanFreq()-X_Mean" | "numeric" | "-0.395770150677419  /  0.249209411510602" | "-0.104551025495773"
"68" | "Fast Fourier Body gyroscope meanFreq()-Y_Mean" | "numeric" | "-0.666814815306122  /  0.273141323315789" | "-0.167407475856434"
"69" | "Fast Fourier Body gyroscope meanFreq()-Z_Mean" | "numeric" | "-0.507490866734694  /  0.3770740968" | "-0.0571809440547551"
"70" | "Fast Fourier Body accelerometer Magnitude mean()_Mean" | "numeric" | "-0.986800645362319  /  0.586637550769231" | "-0.536516692548498"
"71" | "Fast Fourier Body accelerometer Magnitude std()_Mean" | "numeric" | "-0.987648484461539  /  0.178684580868889" | "-0.620963293005196"
"72" | "Fast Fourier Body accelerometer Magnitude meanFreq()_Mean" | "numeric" | "-0.312338030213846  /  0.435846931652174" | "0.0761281754555899"
"73" | "Fast Fourier Body accelerometer Jerk Magnitude mean()_Mean" | "numeric" | "-0.993998275797101  /  0.538404846128205" | "-0.575617493234432"
"74" | "Fast Fourier Body accelerometer Jerk Magnitude std()_Mean" | "numeric" | "-0.994366667681159  /  0.316346415348718" | "-0.599160868317743"
"75" | "Fast Fourier Body accelerometer Jerk Magnitude meanFreq()_Mean" | "numeric" | "-0.125210388757581  /  0.488088499666667" | "0.162545885494571"
"76" | "Fast Fourier Body gyroscope Magnitude mean()_Mean" | "numeric" | "-0.986535242105263  /  0.203979764835897" | "-0.667099099613148"
"77" | "Fast Fourier Body gyroscope Magnitude std()_Mean" | "numeric" | "-0.981468841692308  /  0.236659662496296" | "-0.672322349574843"
"78" | "Fast Fourier Body gyroscope Magnitude meanFreq()_Mean" | "numeric" | "-0.456638670923077  /  0.409521611525424" | "-0.0360322479939937"
"79" | "Fast Fourier Body gyroscope Jerk Magnitude mean()_Mean" | "numeric" | "-0.997617389275362  /  0.146618569064407" | "-0.756385271117363"
"80" | "Fast Fourier Body gyroscope Jerk Magnitude std()_Mean" | "numeric" | "-0.99758523057971  /  0.287834616098305" | "-0.771517051737343"
"81" | "Fast Fourier Body gyroscope Jerk Magnitude meanFreq()_Mean" | "numeric" | "-0.182923596577778  /  0.426301679855072" | "0.125922459004982"

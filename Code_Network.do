clear all
global path_1="C:\Users\Lenovo\Desktop\new_intitution_0923\save-2_保存的VaR" /*VaR & 合并的残差所在的路径*/
global path_2="C:\Users\Lenovo\Desktop\new_intitution_0923\save-3_LASSO系数"
global path_3="C:\Users\Lenovo\Desktop\new_intitution_0923\save-3_LASSO系数\DAT化的LASSO参数"
global path_4="C:\Users\Lenovo\Desktop\new_intitution_0923\save-4_网络输出的结果"


*-------将 lambda_Lx.csv 文件DTA化-------*

forvalue i=1/38 {
	cd $path_2
	import delimited lambda_L`i'.csv,clear
	rename v1 time 
	rename v2 lambda_`i'
	cd $path_3
	save lambda_`i'.dta,replace
	}

*-------将 beta_newx.csv 文件DTA化-------*

forvalue i=1/38 {
	cd $path_2
	import delimited beta_new`i'.csv,clear
	
	rename v1 time
	rename v2 beta_constant
	gen    row=`i'

	qui numlist  "1/38",integer ascending
	*dis  "`r(numlist)'"
	local newname  `r(numlist)'
	*dis "`newname'"
	local newname_remove_i: list newname-i
	*dis "`newname_remove_i'"

	qui des v3-v39,varlist
	local  oldname  `r(varlist)'

	forvalue j=1/37 {
		local a:word `j' of `oldname'
		local b:word `j' of `newname_remove_i'	
		rename `a'  col_`b'
	}
	gen col_`i'=0
	aorder
	cd $path_3
	merge 1:1 time using lambda_`i'.dta
	drop _merge
	rename lambda_`i'  lambda
	
	save beta_new_`i'.dta,replace
}

*-------将 CoVaR_Lx.csv 文件DTA化-------*

forvalue i=1/38 {
	cd $path_2
	import delimited CoVaR_L`i'.csv,clear
	rename v1 time 
	rename v2 CoVaR_L
	cd $path_3
	save CoVaR_L`i'.dta,replace
	}

*-------将 VaR_5_GARCH-HS-金融分业.csv 和 VaR_50_GARCH-HS-金融分业.csv 文件DTA化-------*

cd $path_1
/* 5% of VaR data */
import excel using "VaR_5_合并.xlsx", clear firstrow

gen Date=dofc(date)
format Date %dCY-N-D
drop date
ren Date date
renames VaR_5_*\five_VaR_1-five_VaR_38 /* renames yyds! */
gen time=_n
order time date five_VaR_*
cd $path_3
save 5_VaR.dta,replace

cd $path_1
/* 50% of VaR data */
import excel using "VaR_50_合并.xlsx", clear firstrow

gen Date=dofc(date)
format Date %dCY-N-D
drop date
ren Date date
renames VaR_50_*\fifty_VaR_1-fifty_VaR_38 /* renames yyds! */
gen time=_n
order time date fifty_VaR_*
cd $path_3
save 50_VaR.dta,replace

*----/////-----*
*-------将 all_Industry_resid_data.xlsx 文件DTA化-------*

cd $path_1

import excel using "all_Industry_resid_data_num.xlsx",firstrow clear
gen Date=date(date,"YMD")
format Date %dCY-N-D
drop date
ren Date date
gen time=_n
order time date R_*
cd $path_3
save all_Industry_resid_data.dta,replace

*-------计算CoVaR,用beta_new*乘以相应的变量,beta_new比beta_l多了一列,这一列是截距项-------*

cd  $path_3
use all_Industry_resid_data.dta,clear
 
keep in 250/4546
keep date

merge 1:1 date using  5_VaR.dta
drop _merge
*----/////-----*
cd  $path_3
use 5_VaR.dta, clear



forvalue j=1/38 {

    preserve

	merge 1:1 time using beta_new_`j'.dta
	drop _merge


	gen CoVaR=beta_constant
	forvalue i=1/38 {
		replace CoVaR=col_`i'*five_VaR_`i'+CoVaR
		}

	merge 1:1 time using CoVaR_L`j'.dta
	drop _merge
	
	order time date lambda CoVaR  CoVaR_L row beta_constant col_* beta_* five_VaR_*

	save 自己计算的CoVaR_`j'.dta,replace
	
	restore
}
use 自己计算的CoVaR_1.dta,clear

*-------计算Delta_CoVaR-------*

forvalue i=2/38 {	
	append using 自己计算的CoVaR_`i'.dta
}

merge m:1 time using 50_VaR.dta
drop _merge


*计算dCoVaR*
forvalue j=1/38 {
	gen dCoVaR_`j'=col_`j'*(fifty_VaR_`j'-five_VaR_`j')
}
sum dCoVaR*

*将j对j的影响设置成缺失值.
forvalue j=1/38 {
	replace dCoVaR_`j'=. if row==`j'
}
sum dCoVaR*

keep time date row  lambda dCoVaR*

sreshape long dCoVaR_,i(date time row) j(source)
rename row target
rename dCoVaR_ dcovar


label define name   1 "专用设备" ///
2 "交运仓储" ///
3 "仪器仪表" ///
4 "住宿餐饮" ///
5 "保险" ///
6 "其他金融" ///
7 "农林牧渔" ///
8 "化学原料和化学制品" ///
9 "卫生社会" ///
10 "建筑业" ///
11 "房地产" ///
12 "批发零售" ///
13 "教育" ///
14 "文化体育" ///
15 "木材加工品和家具" ///
16 "水利、环境和公共设施管理" ///
17 "水生产供应" ///
18 "煤炭开采和洗选" ///
19 "燃气生产供应" ///
20 "电气机械和器材制造" ///
21 "电热生产供应" ///
22 "石油天然气开采" ///
23 "石油炼焦和核燃料" ///
24 "科研技术" ///
25 "租赁商务" ///
26 "纺织" ///
27 "纺织服装服饰" ///
28 "计算机通信和电子设备" ///
29 "货币金融服务" ///
30 "资本市场服务" ///
31 "软件信息技术服务" ///
32 "运输设备制造" ///
33 "通用设备" ///
34 "造纸印刷和文教体育用品" ///
35 "金属制品" ///
36 "金属矿采选产品" ///
37 "非金属" ///
38 "食品制造"

label value source name
label value target name

decode source,gen(source1)
decode target,gen(target1)

sum dcovar if dcovar<0
sum dcovar if dcovar==0
sum dcovar if dcovar>0 & dcovar!=.

order date time source target source1 target1 lambda dcovar
cd $path_4
save Delta_CoVaR.dta,replace

********************************************************************************
*计算行业的 yearly 输出 （用于回归）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen year = year(date)
*gen Year = hofd(date)
*format Year %th

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 2
*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(source1 year)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*




*gsort cat -dcovar

rename dcovar To_spill
gen name1=source1

cd $path_4
save "行业输出_yearly.dta",replace


********************************************************************************
*计算行业的 yearly 输入 （用于回归）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen Year = yofd(date)

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 2
*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(target1 Year)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*




*gsort cat -dcovar

rename dcovar From_spill
gen name1=target1

cd $path_4
save "行业输入_yearly.dta",replace


********************************************************************************
*计算行业的 half-yearly pair-wise spill （用于回归）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen Year = hofd(date)
format %th Year

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 2
*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(source1 target1 Year)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*
********************************************************************************
*计算部门内和部门间的尾部风险溢出水平.
cd $path_4
use Delta_CoVaR.dta,replace
sort date source target
gen Month = mofd(date)
format Month %tm

recode source (1/4=2) (5/6=1) (7/10=2) (11/11=1) (12/28=2) (29/30=1) (31/38=2),gen(source2)
recode target (1/4=2) (5/6=1) (7/10=2) (11/11=1) (12/28=2) (29/30=1) (31/38=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
/*replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)*/

collapse (sum) dcovar, by(source2 target2 Month)

*考虑实体部门和金融部门行业数量不同.
replace dcovar=dcovar/(5*4) if source2==1& target2==1
replace dcovar=dcovar/(5*33) if source2==1& target2==2

replace dcovar=dcovar/(33*5)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2


label define sector   1 "金融(包括房地产)" 2 "实体经济"
label value source2 sector
label value target2 sector
set scheme plotplainblind
tw  line dcovar Month   ,by(source2 target2,yrescale) yline(0, lc(red)) 

grss tw line dcovar date if source2==2 & target2==1

tabulate source2 target2,sum(dcovar)

save 部门内和部门间.dta,replace
********************************************************************************
*计算实体行业→Finance的yearly 输出 （用于回归）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen year = year(date)
*gen Year = hofd(date)
*format Year %th

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 1
*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(source1 year)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*




*gsort cat -dcovar

rename dcovar To_spill
gen name1=source1

cd $path_4
save "行业_to_Finance_yearly.dta", replace


*gsort cat -dcovar

rename dcovar Pair_spill
*********************溢出方为上游时的投入产出关系******************************
global path_trcc = "C:\Users\Lenovo\Desktop\溢出\DTA化的投入产出表（挑战杯）"

cd $path_trcc
ren source1 上游
ren target1 下游
merge m:1 上游 下游 using "2015"
keep if _merge == 3
drop _merge
local name = "_溢出方为上游"
ren CI CI`name'
ren CO CO`name'

ren 下游 target1
ren  上游 source1
ren source1 下游
ren target1 上游

*********************溢出方为下游时的投入产出关系******************************
cd $path_trcc
merge m:1 下游 上游 using "2015"
keep if _merge == 3
drop _merge
local name = "_溢出方为下游"
ren CI CI`name'
ren CO CO`name'

ren 下游 source1 
ren 上游 target1 
cd $path_4
save "Pair_yearly.dta",replace

********************************************************************************
*计算行业的 half-yearly (+) 输出 （用于回归）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen Year = hofd(date)
format Year %th

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 2
*去除(-)溢出，保留(+)溢出*
drop if dcovar <= 0
*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(source1 Year)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*




*gsort cat -dcovar

rename dcovar To_spill
gen name1=source1

cd $path_4
save "行业输出_yearly.dta",replace

********************************************************************************
*计算实体部门内部的关联水平.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target
gen Month = mofd(date)
format Month %tm

*将自己对自己去掉先*
drop if dcovar==. 
gen cat=1	
replace cat=2 if strmatch(source1,"其他金融")
replace cat=2 if strmatch(source1,"房地产")
replace cat=2 if strmatch(source1,"货币金融服务")
replace cat=2 if strmatch(source1,"资本市场服务")
replace cat=2 if strmatch(source1,"保险")
drop if cat == 2
drop cat
gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")
replace cat=2 if strmatch(target1,"保险")
drop if cat == 2

*keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))
* Choice 1:
collapse (sum) dcovar, by(Month)
* Choice 2:
*bysort source1:gen N=_N
*collapse (sum) dcovar (mean) N,by(source1 Year)
*replace dcovar=dcovar/N
*
replace dcovar = dcovar / (33 * 32)
tw line dcovar Month


*gsort cat -dcovar

rename dcovar To_spill
gen name1=source1

cd $path_4
save "行业输出_yearly.dta",replace

********************************************************************************

/*
*周度*
collapse (sum) dcovar,by(source1 week)
gsort week -dcovar

*季度*
gen q=qofd(week)
format q %tq
collapse (mean) dcovar,by(source1 q)
gsort q -dcovar
*/

*计算行业的输入.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

*将自己对自己去掉先*
drop if dcovar==. 

keep if (date>=date("2018-1-1","YMD") & date <= date("2018-12-1","YMD"))

bysort target1:gen N=_N
collapse (sum) dcovar (mean) N,by(target1)

replace dcovar=dcovar/N

gen cat=1	
replace cat=2 if strmatch(target1,"其他金融")
replace cat=2 if strmatch(target1,"房地产")
replace cat=2 if strmatch(target1,"货币金融服务")
replace cat=2 if strmatch(target1,"资本市场服务")

gsort cat -dcovar

rename dcovar dcovar_in
gen name2=target1

cd $path_4
save 行业输入.dta,replace


cd "C:\Users\Lenovo\Desktop\溢出\投入产出表0323（股票）"
import excel using "2018D.xlsx",firstrow clear
cd $path_4
merge 1:1 name1 using "行业输出.dta"
keep if _merge == 3
drop _merge

merge 1:1 name2 using "行业输入.dta"
keep if _merge == 3
drop _merge
set scheme plotplain
gen dd = ln(D)
gen oo = ln(out)
tw scatter dcovar_in dd
tw (scatter dcovar_out oo) (lfit dcovar_out oo)
tw (scatter dcovar_out ii) (lfit dcovar_out ii)
tw (scatter dcovar_in oo) (lfit dcovar_in oo)
tw (scatter dcovar_in ii) (lfit dcovar_in ii)

reg dcovar_in ii
reg dcovar_in oo
reg dcovar_out oo
reg dcovar_out ii

/*
*周度
collapse (sum) dcovar,by(target1 week)
gsort week -dcovar

*季度*
gen q=qofd(week)
format q %tq
collapse (mean) dcovar,by(target1 q)
gsort q -dcovar
*/

*输出与输入作图*
cd $path_4
use 行业输入.dta,clear
merge 1:1 name using 行业输出.dta

tw scat dcovar_in dcovar_out,mlab(name)

tw scat dcovar_in dcovar_out,mlab(name) xtitle(out) ytitle(in)





*网络结构*

cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

collapse  (sum) dcovar,by(source1 target1)
gsort -dcovar

drop if dcovar==0
sum dcovar

drop if dcovar<r(mean)

*从大到小*
gsort -dcovar
save 网络结构从大到小.dta,replace


*按照source从大到小*
gsort source1 -dcovar
save 网络结构按照source从大到小.dta,replace


*按照target从大到小*
gsort target1 -dcovar
save 网络结构按照target从大到小.dta,replace


netplot source1 target1,label  type(mds)
netplot source1 target1,label  type(circle)


*计算系统的尾部风险溢出水平*
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

*因为没有边的观测也没有删,所以每个target机构出现的次数是一样的.
collapse (mean) lambda (sum) dcovar_sum=dcovar,by(date)
replace dcovar_sum=dcovar_sum/(37*36)


tw (connected dcovar_sum date),xsize(6) ysize(3)
tw (line lambda date)         ,xsize(6) ysize(3)
tw (line dcovar_sum date) (line lambda date,yaxis(2)),xsize(6) ysize(3)
save 系统总体日度.dta,replace


preserve
gen q=qofd(date)
format q %tq
collapse (mean) lambda dcovar_sum,by(q)

gen quarter=quarter(dofq(q))

tw connected lambda q    ,xsize(6) ysize(3)   mlabel(quarter)
tw connected dcovar_sum q,xsize(6) ysize(3)   mlabel(quarter)
grss tw (connected dcovar_sum q) (connected lambda q,yaxis(2)),xsize(6) ysize(3) 

save 系统总体季度.dta,replace
restore 

*计算部门内和部门间的尾部风险溢出水平.
cd $path_4
use Delta_CoVaR.dta,replace
sort date source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=1) (11/27=2) (28/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=1) (11/27=2) (28/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

collapse (sum) dcovar  ,by(source2 target2 date)

*考虑实体部门和金融部门行业数量不同.
replace dcovar=dcovar/(4*3) if source2==1& target2==1
replace dcovar=dcovar/(4*33) if source2==1& target2==2

replace dcovar=dcovar/(33*4)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2

format date %dCY

label define sector   1 "金融(包括房地产)" 2 "实体经济"
label value source2 sector
label value target2 sector

grss tw  line dcovar date   ,by(source2 target2,yrescale)

grss tw line dcovar date if source2==2 & target2==1

tabulate source2 target2,sum(dcovar)

save 部门内和部门间.dta,replace


*计算部门内和部门间的尾部风险溢出水平（月度）.
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=1) (11/27=2) (28/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=1) (11/27=2) (28/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

gen Month=mofd(date)
format Month %tm

collapse (mean) dcovar  ,by(source2 target2 Month)

*考虑实体部门和金融部门行业数量不同.
replace dcovar=dcovar/(4*3) if source2==1& target2==1
replace dcovar=dcovar/(4*33) if source2==1& target2==2

replace dcovar=dcovar/(33*4)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2



label define sector   1 "金融(包括房地产)" 2 "实体经济"
label value source2 sector
label value target2 sector

grss tw  line dcovar Month   ,by(source2 target2,yrescale)

grss tw line dcovar Month if source2==2 & target2==1

tabulate source2 target2,sum(dcovar)

save 部门内和部门间（月度）.dta,replace

*计算部门内和部门间的尾部风险溢出水平.(房地产单独作为一个部门，月度)
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

gen Month=mofd(date)
format Month %tm

collapse (mean) dcovar  ,by(source2 target2 Month)

*考虑实体部门、金融部门和房地产部门行业数量不同.
replace dcovar=dcovar/(3*33) if source2==1& target2==2
replace dcovar=dcovar/(3*1) if source2==1& target2==3

replace dcovar=dcovar/(33*3)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2
replace dcovar=dcovar/(33*1)  if source2==2& target2==3

replace dcovar=dcovar/(1*3)  if source2==3& target2==1
replace dcovar=dcovar/(1*32)  if source2==3& target2==2


label define sector   1 "金融" 2 "实体经济" 3 "房地产"
label value source2 sector
label value target2 sector

grss tw  line dcovar Month   ,by(source2 target2,yrescale)

tabulate source2 target2,sum(dcovar)

save 部门内和部门间（把房地产作为一个独立的部门，月度）.dta,replace

*计算部门内和部门间的尾部风险溢出水平.(房地产、银行单独作为一个部门，月度)
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/28=4) (29/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/28=4) (29/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

gen Month=mofd(date)
format Month %tm

collapse (mean) dcovar  ,by(source2 target2 Month)

*考虑实体部门、金融部门和房地产部门行业数量不同.

/*非银行金融对其他*/
replace dcovar=dcovar/(2*33) if source2==1& target2==2
replace dcovar=dcovar/(2*1) if source2==1& target2==3
replace dcovar=dcovar/(2*1) if source2==1& target2==4
replace dcovar=dcovar/(2*1) if source2==1& target2==1
/*实体对其他*/
replace dcovar=dcovar/(33*2)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2
replace dcovar=dcovar/(33*1)  if source2==2& target2==3
replace dcovar=dcovar/(33*1)  if source2==2& target2==4
/*房地产对其他*/
replace dcovar=dcovar/(1*2)  if source2==3& target2==1
replace dcovar=dcovar/(1*33)  if source2==3& target2==2
replace dcovar=dcovar/(1*1)  if source2==3& target2==4
/*银行对其他*/
replace dcovar=dcovar/(1*2)  if source2==4& target2==1
replace dcovar=dcovar/(1*33)  if source2==4& target2==2
replace dcovar=dcovar/(1*1)  if source2==4& target2==3


label define sector   1 "非银行金融" 2 "实体经济" 3 "房地产" 4 "银行"
label value source2 sector
label value target2 sector

grss tw  line dcovar Month   ,by(source2 target2,yrescale)

grss tw line dcovar Month if source2==2 & target2==4

grss tw (connected dcovar Month if source2==4 & target2==2) (connected dcovar Month if source2==2 & target2==4),xsize(6) ysize(3) 


tabulate source2 target2,sum(dcovar)

save 部门内和部门间（把银行、房地产作为一个独立的部门，月度）.dta,replace

*计算部门内和部门间的尾部风险溢出水平.(房地产、银行单独作为一个部门，日频)
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/28=4) (29/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/28=4) (29/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)



collapse (sum) dcovar  ,by(source2 target2 date)

*考虑实体部门、金融部门和房地产部门行业数量不同.

/*非银行金融对其他*/
replace dcovar=dcovar/(2*33) if source2==1& target2==2
replace dcovar=dcovar/(2*1) if source2==1& target2==3
replace dcovar=dcovar/(2*1) if source2==1& target2==4
replace dcovar=dcovar/(2*1) if source2==1& target2==1
/*实体对其他*/
replace dcovar=dcovar/(33*2)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2
replace dcovar=dcovar/(33*1)  if source2==2& target2==3
replace dcovar=dcovar/(33*1)  if source2==2& target2==4
/*房地产对其他*/
replace dcovar=dcovar/(1*2)  if source2==3& target2==1
replace dcovar=dcovar/(1*33)  if source2==3& target2==2
replace dcovar=dcovar/(1*1)  if source2==3& target2==4
/*银行对其他*/
replace dcovar=dcovar/(1*2)  if source2==4& target2==1
replace dcovar=dcovar/(1*33)  if source2==4& target2==2
replace dcovar=dcovar/(1*1)  if source2==4& target2==3


label define sector   1 "非银行金融" 2 "实体经济" 3 "房地产" 4 "银行"
label value source2 sector
label value target2 sector

grss tw  line dcovar date   ,by(source2 target2,yrescale)

grss tw line dcovar date if source2==2 & target2==4

grss tw (connected dcovar date if source2==4 & target2==2) (connected dcovar date if source2==2 & target2==4),xsize(6) ysize(3) 


tabulate source2 target2,sum(dcovar)

save 部门内和部门间（把银行、房地产作为一个独立的部门，月度）.dta,replace


*计算部门内和部门间的尾部风险溢出水平.(房地产单独作为一个部门)
cd $path_4
use Delta_CoVaR.dta,replace
sort date time source target

recode source (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/29=1) (30/37=2),gen(source2)
recode target (1/4=2) (5/5=1) (6/9=2) (10/10=3) (11/27=2) (28/29=1) (30/37=2),gen(target2)

*将金融行业之间的负向溢出设为缺失值
replace dcovar=. if dcovar<=0 & (target==5 & source==28)
replace dcovar=. if dcovar<=0 & (target==5 & source==29)
replace dcovar=. if dcovar<=0 & (target==28 & source==5)
replace dcovar=. if dcovar<=0 & (target==28 & source==29)
replace dcovar=. if dcovar<=0 & (target==29 & source==5)
replace dcovar=. if dcovar<=0 & (target==29 & source==28)

gen quarter=qofd(date)

format quarter %tq

collapse (mean) dcovar  ,by(source2 target2 quarter)

*考虑实体部门、金融部门和房地产部门行业数量不同.
replace dcovar=dcovar/(3*33) if source2==1& target2==2
replace dcovar=dcovar/(3*1) if source2==1& target2==3

replace dcovar=dcovar/(33*3)  if source2==2& target2==1
replace dcovar=dcovar/(33*32)  if source2==2& target2==2
replace dcovar=dcovar/(33*1)  if source2==2& target2==3

replace dcovar=dcovar/(1*3)  if source2==3& target2==1
replace dcovar=dcovar/(1*32)  if source2==3& target2==2



label define sector   1 "金融" 2 "实体经济" 3 "房地产"
label value source2 sector
label value target2 sector

grss tw  line dcovar year   ,by(source2 target2,yrescale)

grss tw line dcovar quarter if source2==2 & target2==1
grss tw line dcovar Month if source2==3 & target2==2

tabulate source2 target2,sum(dcovar)

save 部门内和部门间（把房地产作为一个独立的部门）.dta,replace


********************************************************************************
*计算部门输出.
cd $path_4
use Delta_CoVaR.dta,replace
sort week time source target

*将dcovar<=0的设为缺失值.
replace dcovar=. if dcovar<=0

recode source (1/8=2) (9/9=1) (10/32=2) (33/33=1) (34/35=2),gen(source2)

collapse (sum) dcovar  ,by(source2 week)

*考虑实体部门和金融部门行业数量不同.
replace  dcovar =  dcovar/(2*34)  if source2==1
replace  dcovar =  dcovar/(33*34)  if source2==2


tw  line dcovar week   ,by(source2)

tabulate source2,sum(dcovar)

save 部门source.dta,replace




********************************************************************************
*计算部门输入.
cd $path_4
use Delta_CoVaR.dta,replace
sort week time source target

*将dcovar<=0的设为缺失值.
replace dcovar=. if dcovar<=0

recode source (1/8=2) (9/9=1) (10/32=2) (33/33=1) (34/35=2),gen(target2)

collapse (sum)  dcovar  ,by(target2 week)

*考虑实体部门和金融部门行业数量不同
replace  dcovar =  dcovar/(2*30)  if target2==1
replace  dcovar =  dcovar/(33*30)  if target2==2


tw  line dcovar week    ,by(target2,yrescale)

tabulate target2,sum(dcovar)

save 部门target.dta,replace


********************************************************************************
*Systemic Risk Receiver Index*
cd $path_4
use Delta_CoVaR.dta,replace
sort week time source target

*将dcovar<=0的设为缺失值.
replace dcovar=. if dcovar<=0

gen  srr1= dcovar
egen srr2=total(srr1),by(week target1)

gen  srr3=srr2

collapse (mean) srr3,by(week target1)

gsort week -srr3
save SRR.dta,replace




*Systemic Risk Emitter Index*
cd $path_4
use Delta_CoVaR.dta,replace
sort week time source target

*将dcovar<=0的设为缺失值.
replace dcovar=. if dcovar<=0

gen  sre1=dcovar
egen sre2=total(sre1),by(week source1)
gen  sre3=sre2
collapse (mean) sre3,by(week source1)
gsort week -sre3
save SRE.dta,replace

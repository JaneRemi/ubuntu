## Author: Sophia Jiao 
## Date: 01/11/2017
## independent variable: 1) 烧结铁燃辅料+球团铁辅料+块矿燃辅料;
##                       2) 烧结内返/ 球团外返(t);
##                       3) 烧结/球团每日生产余量(t)
## Mainly change: 1) 完善整个分析（没考虑烧结、球团外售） + 返矿部分函数化

Sys.setlocale("LC_ALL","Chinese")
setwd("C:/AA-Projects/optimix/model")
library(dplyr)
library(data.table)
library(nloptr)
library(purrr)


## Session 1. raw data read-in and basic check
read_and_fix_na <- function(path){
  x <- read.csv(path, header=TRUE,stringsAsFactors =F, sep=',')
  x[is.na(x)] = 0
  return(x)}
check_numeric <- function(df,col1,col2){
  if (col2 == 0){
    a <- map(df[,col1:ncol(df)],is.numeric)
    if (FALSE %in% a){ message("some numeric columns are inputed as charaters") }
  } else {
    a <- map(df[,col1:col2],is.numeric)
    if (FALSE %in% a){ message("some numeric columns are inputed as charaters") }
  }}
adj_coef <- read_and_fix_na(path = './input/adj_coef.csv'); check_numeric(adj_coef,3,0)
constant <- read_and_fix_na(path = './input/constant.csv'); check_numeric(constant,4,4)
cons <- read_and_fix_na(path = './input/constraint.csv'); check_numeric(cons,4,5)
form_coef <- read_and_fix_na(path = './input/formula_coef.csv'); check_numeric(form_coef,4,5)
market <- read_and_fix_na(path = './input/market.csv'); check_numeric(market,5,0)
x_defi <- read_and_fix_na(path = './input/x_defi.csv'); check_numeric(x_defi,4,6)


############################### set initial value of x ##################################################
#x <- x_defi[1:39,"初始值"]
##########################################################################################################

# check No. of independent variables
x_count <- nrow(market) 
message("Total number of independent variables is ", x_count)


## Session 2. Function defined
# 1) 入料系数函数：用于求各process中入料对应系数的函数。输入变量：pro - 流程；cate - 大类
ruliao <- function(pro,cate){           
  m <- which(market$流程 == pro & market$大类 == cate)
  c <- rep(0,x_count)
  for (i in 1:x_count){
    if (i %in% m){
      c[i] <- 1
    } else {
      c[i] <- 0
    }
  }
  return(c)
}
# 2) 价格、元素、成分加权平均总和：输入变量：col - 所求对象，如价格/ TFe; pro - 流程；cate - 大类
# Note: 除了价格外，其他都做了百分比的转换
# 若只需要精度到流程（即烧结、球团、块矿）不需要到具体铁燃辅料，则cate=ALL
price_elem <- function(pro, cate, col){
  if (cate == "ALL") {
    m <- which(market$流程 == pro )
  } else {
    m <- which(market$流程 == pro & market$大类 == cate)
  }
  c <- rep(0,x_count)
  for (i in 1:x_count){
    if (i %in% m){
      c[i] <- market[[i,col]]
    } else {
      c[i] <- 0
    }
  }
  if (col == "price.干基.") { c <- c } else { c <- c/100 }
  return(c)
}
# 3) 生成物系数函数，已做了百分比的转化
product <- function(pro,cate){           
  m <- which(market$流程 == pro & market$大类 == cate)
  c <- rep(0,x_count)
  for (i in 1:x_count){
    if (i %in% m){
      c[i] <- 1 - market$烧损LOI[i]/100
    } else {
      c[i] <- 0
    }
  }
  return(c)
}
# 4) constant文件提取数据
# pro对应‘流程’列具体数值; cate对应‘大类’列具体数值
cons_get_data <- function(pro,cate){
  m <- which(constant$流程== pro & constant$大类== cate)
  b <- sum(constant$数值[m])
  return(b)
}

# 5) 
get_data <- function(pro,cate,item,value){
  m <- which(form_coef$流程== pro & form_coef$大类== cate & form_coef$项目==item)
  b <- form_coef[m,value]
  return(b)
}


## Section 3. Detailed calculation by production process
## 1) sinter
# 1.1) 入料
s_tie_ru <- ruliao(pro = "烧结", cate = "铁料")
s_ran_ru <- ruliao(pro = "烧结", cate = "燃料")
s_fu_ru <- ruliao(pro = "烧结", cate = "辅料")
s_ru_1 <- s_tie_ru + s_ran_ru + s_fu_ru
# 1.2) 生成物
s_tie_pro <- product(pro = "烧结", cate = "铁料")
s_ran_pro <- product(pro = "烧结", cate = "燃料")
s_fu_pro <- product(pro = "烧结", cate = "辅料")
s_pro <- s_tie_pro + s_ran_pro + s_fu_pro
# 1.3) 化学元素
# 求生成烧结矿中各元素的质量（未考虑返矿前），只需更新以下col对应值，cate保持为ALL
s_ele_fun <- function(element){ 
  function(x){
    test1 <- price_elem(pro = "烧结", cate = "ALL", col = element) %*% x
    return(as.numeric(test1))
  }
}
# 1.4) 价格
s_price <- price_elem(pro="烧结",cate="ALL",col="price.干基.")
s_fixed_cost <- cons_get_data(pro = "烧结", cate = "日均成本")  # 加总为烧结部分的单日总固定成本
# 1.5) 其他调整
# 褐铁矿
index_hetie <- which(market$矿石种类=="褐铁矿")
hetie <- rep(0,x_count)
hetie[index_hetie] <- 1
s_hetie_per_fun <- function(x){ 
  test <- (hetie %*% x)/(s_tie_ru %*% x)
  return(as.numeric(test))  
}
# 石灰石
index_limestone <- which(market$项目=="石灰石")
limestone <- rep(0,x_count)
limestone[index_limestone] <- 1

## 2) pellet
# 2.1) 入料
p_tie_ru <- ruliao(pro = "球团", cate = "铁料")
p_fu_ru <- ruliao(pro = "球团", cate = "辅料")
p_ru <- p_tie_ru + p_fu_ru
# 2.2) 生成物
p_tie_pro <- product(pro = "球团", cate = "铁料")
p_fu_pro <- product(pro = "球团", cate = "辅料")
p_pro <- p_tie_pro + p_fu_pro
# 2.3) 化学元素
# 求生成球团矿中各元素的质量（未考虑返矿前），只需更新以下col对应值，cate保持为ALL
p_ele_fun <- function(element){ 
  function(x){
    test2 <- price_elem(pro = "球团", cate = "ALL", col = element) %*% x
    return(as.numeric(test2))
  }
}
# 2.4) 价格
p_price <- price_elem(pro="球团",cate="ALL",col="price.干基.")
p_ran_cost <- cons_get_data(pro = "球团", cate = "成本") * p_pro # 燃料成本和球团矿产量相关
p_fixed_cost <- cons_get_data(pro = "球团", cate = "日均成本")  # 加总为烧结部分的单日总固定成本
# 2.5) 其他调整
# 黏土总用量
niantu_index = which(market$流程 == "球团" & market$大类 == "辅料")
temp <- rep(0,x_count)
temp[niantu_index] <- 1
p_niantu <- temp
# 辅料限制参数矩阵
p_fu_eq <- cons_get_data(pro = "球团", cate = "消耗") * p_pro
p_fu_eq_fun <- function(x){
  test <- (p_fu_eq * p_pro) %*% x
  return(as.numeric(test))
}
# 2.6) 球团入料中各原料占比
p_ru_per_fun <- function(element){ 
  function(x){
    test_list <- rep(0,x_count)
    index_test <- which(market$项目==element)
    test_list[index_test] <- 1
    if (market$流程[index_test]=="球团"){
      test <- (test_list %*% x)/(p_pro %*% x)
      return(as.numeric(test)) 
    } else {
      message("The input doesn't belong to the Pellet process, pls. double check!") 
    }
  }
}

# 2.7) 球团外返
p_waifan_eq <- function(x){
  base <- get_data("球团","外返","基准比例","基准值")
  niantu_consup <- (p_niantu %*% x) / (p_pro %*% x)
  niantu_n <- get_data("球团","外返","黏土","系数") *(as.numeric(niantu_consup) - get_data("球团","外返","黏土","基准值"))
  test0 <- base + niantu_n
  test <- test0 * (p_pro %*%x)
  return(as.numeric(test))
}
p_waifan_rate <- function(x){
  test <- p_waifan_eq(x)/ (p_pro %*%x)
  return(as.numeric(test))
}


## 3) blast furnance <1>
# 3.1) 外购铁料：入炉块矿。外购球团、外购烧结
# a) 入料
b_tie_waigou_ru <- ruliao(pro = "高炉", cate = "块矿") + 
  ruliao(pro = "高炉", cate = "球团") +
  ruliao(pro = "高炉", cate = "烧结")
# b) 生成物，即烧损
b_tie_waigou_pro <- product(pro="高炉",cate="块矿") +
  product(pro="高炉",cate="球团") +
  product(pro="高炉",cate="烧结") 
# c) 化学元素
b_waigou_ele_fun <- function(element){ 
  function(x){
    test3 <- (price_elem(pro = "高炉", cate = "块矿", col = element) + 
                price_elem(pro = "高炉", cate = "球团", col = element) + 
                price_elem(pro = "高炉", cate = "烧结", col = element)) %*% x
    return(as.numeric(test3))  
  }
}
# d) 返矿
b_tie_fan <- price_elem(pro="高炉",cate="块矿",col="返粉率") +
  price_elem(pro="高炉",cate="球团",col="返粉率") +
  price_elem(pro="高炉",cate="烧结",col="返粉率") 

b_waifan_fun <- function(x){
  test <- b_tie_fan %*% x
  return(as.numeric(test))
}


# Section 4. Sinter iteration
# base function, 含原来配方返矿
s_ru_fun_1 <- function(x){
  return(as.numeric(s_ru_1 %*% x))
}
s_pro_fun_1 <- function(x){
  return(as.numeric(s_pro %*% x))
}
s_ele_fun_1 <- function(element){ 
  function(x){
    test1 <- price_elem(pro = "烧结", cate = "ALL", col = element) %*% x
    return(as.numeric(test1))
  }
}
# 内返(质量)
s_neifan_deltaele_fun_1 <- function(element){
  function(x){                                        # s_ele_fun_1需要更新
    test <- get_data("烧结","内返",element,"系数") * (s_ele_fun_1(element)(x) - get_data("烧结","内返",element,"基准值"))
    return(as.numeric(test))  
  }
}
s_neifan_eq_1 <- function(x){
  base <- get_data("烧结","内返","基准比例","基准值")
  SiO2_n <- s_neifan_deltaele_fun_1("SiO2")(x)
  hetiekuang <- get_data("烧结","内返","褐铁矿","系数") * (s_hetie_per_fun(x) - get_data("烧结","内返","褐铁矿","基准值"))
  Al2O3_n <- s_neifan_deltaele_fun_1("Al2O3")(x)
  CaO_n <- s_neifan_deltaele_fun_1("CaO")(x)
  test0 <- base + SiO2_n + hetiekuang + Al2O3_n + CaO_n 
  test <- test0 * (s_ru_fun_1(x))
  return(as.numeric(test))
}
# 外返(质量)
s_waifan_eq_1 <- function(x){
  base <- get_data("烧结","外返","基准外返比率","基准值")            # 内返 / 入料 修改
  test0 <- base + get_data("烧结","外返","基准内返比率","系数") * (s_neifan_eq_1(x)/(s_ru_fun_1(x)) - get_data("烧结","外返","基准内返比率","基准值"))
  test <- test0 * s_pro_fun_1(x)
  return(as.numeric(test))
}
# 元素含量
s_eleper_fun_1 <- function(element){ 
  function(x){
    test1 <- s_ele_fun_1(element)(x) / s_pro_fun_1(x)
    return(as.numeric(test1))
  }
}
# 变料后函数
s_ru_fun_2 <- function(neifan,waifan){
  function(x){
    index <- s_ru_1
    index[1] <- 0
    s_ru_2 <- index %*% x + neifan + waifan + p_waifan_eq(x) + b_waifan_fun(x)
    return(as.numeric(s_ru_2))
  }}
s_pro_fun_2 <- function(neifan,waifan){
  function(x){
    index <- s_pro
    index[1] <- 0
    s_pro_2 <- index %*% x + neifan + waifan + p_waifan_eq(x) + as.numeric((b_tie_waigou_pro*b_tie_fan) %*% x)
    return(as.numeric(s_pro_2))
  }}
s_ele_fun_2 <- function(neifan,waifan,s_ru,nrow,df1){
  function(element){ 
    function(x){
      index <- s_ru_1
      index[1] <- 0
      test_tie <- df1[nrow,element] * (index %*% x + neifan + waifan)/s_ru
      test_p <- p_ele_fun(element)(x)*(p_waifan_eq(x)/p_pro %*% x)
      test_b <- b_waigou_ele_fun(element)(x)*(b_waifan_fun(x)/(b_tie_waigou_ru %*% x))
      test1 <- test_tie + test_p  + test_b 
      return(as.numeric(test1))
    }}}
s_neifan_deltaele_fun_2 <- function(nrow,df1){
  function(element){
    function(x){                                        # s_ele_fun_1需要更新
      test <- get_data("烧结","内返",element,"系数") * (df1[nrow,element] - get_data("烧结","内返",element,"基准值"))
      return(as.numeric(test))  
    }}}
s_neifan_eq_2 <- function(nrow,df1,df2){
  function(x){
    base <- get_data("烧结","内返","基准比例","基准值")
    SiO2_n <- df1[nrow,"SiO2"]
    hetiekuang <- get_data("烧结","内返","褐铁矿","系数") * (s_hetie_per_fun(x) - get_data("烧结","内返","褐铁矿","基准值"))
    Al2O3_n <- df1[nrow,"Al2O3"]
    CaO_n <- df1[nrow,"CaO"]
    test0 <- base + SiO2_n + hetiekuang + Al2O3_n + CaO_n 
    test <- test0 * df2[nrow]
    return(as.numeric(test))
  }}
s_waifan_eq_2 <- function(neifan,ruliao,pro){
  function(x){
    base <- get_data("烧结","外返","基准外返比率","基准值")            # 内返 / 入料 修改
    test0 <- base + get_data("烧结","外返","基准内返比率","系数") * (neifan/(ruliao) - get_data("烧结","外返","基准内返比率","基准值"))
    test <- test0 * pro
    return(as.numeric(test))
  }}
s_eleper_fun_2 <- function(nrow,df1,df2){
  function(element){ 
    function(x){
      test1 <- df1[nrow,element] / df2[nrow]
      return(as.numeric(test1))
    }}}
element_change_2 <-  function(nrow,df1){
  function(element){ 
    function(x){
      test1 <- (df1[nrow,element] -df1[(nrow-1),element])/df1[(nrow-1),element]
      return(as.numeric(test1))
    }}}

################################### setting parameter ##############################################
element_group <- colnames(market)[6:18] 
element_2 <- c("SiO2","Al2O3","CaO")
calculate <- function(rule,data){
  function(x){
  test_data <- data
  calculation <- rep(0,length(test_data))
  names(calculation) <- test_data
  for (i in (1:length(test_data))){
    calculation[i] <- rule(test_data[i])(x)
    #print(calculation[i])
  }
  return(calculation)}
}

####################### functions ################################################################

s_fan_interation <- function(x){
  n <- 100
  break_value <- 0.02#; i <- 1
  ########################### initial vector/ matrix to store looping results #########################################
  s_ru_loop <- rep(0,n); s_pro_loop <- rep(0,n); s_neifan_loop <- rep(0,n); s_waifan_loop <- rep(0,n)
  s_ele_loop <- matrix(0, nrow=n, ncol=length(element_group));colnames(s_ele_loop) <- element_group
  s_eleper_loop <- matrix(0, nrow=n, ncol=length(element_group));colnames(s_eleper_loop) <- element_group
  s_neifan_delta_loop <- matrix(0, nrow=n, ncol=length(element_2));colnames(s_neifan_delta_loop) <- element_2
  element_change_loop <- matrix(0, nrow=n, ncol=length(element_group));colnames(element_change_loop) <- element_group
  ####################################################################################################################
  for (i in (1:n)){
    if (i == 1) {
      s_ru_loop[i] <- s_ru_fun_1(x)
      s_pro_loop[i] <- s_pro_fun_1(x)
      s_ele_loop[i,] <- t(as.matrix(calculate(s_ele_fun_1,element_group)(x)))
      s_neifan_delta_loop[i,] <- t(as.matrix(calculate(s_neifan_deltaele_fun_1,element_2)(x))) 
      s_neifan_loop[i] <- s_neifan_eq_1(x)
      s_waifan_loop[i] <- s_waifan_eq_1(x)
      s_eleper_loop[i,] <- t(as.matrix(calculate(s_eleper_fun_1,element_group)(x)))
    } else {
      s_ru_loop[i] <- s_ru_fun_2(s_neifan_loop[(i-1)],s_waifan_loop[(i-1)])(x)
      s_pro_loop[i] <- s_pro_fun_2(s_neifan_loop[(i-1)],s_waifan_loop[(i-1)])(x)
      s_ele_loop[i,] <- t(as.matrix(calculate(s_ele_fun_2(s_neifan_loop[(i-1)],s_waifan_loop[(i-1)],s_ru_loop[(i-1)],(i-1),s_ele_loop),element_group)(x)))
      s_neifan_delta_loop[i,] <- t(as.matrix(calculate(s_neifan_deltaele_fun_2((i-1),s_ele_loop),element_2)(x))) 
      s_neifan_loop[i] <- s_neifan_eq_2(i,s_neifan_delta_loop,s_ru_loop)(x)
      s_waifan_loop[i] <- s_waifan_eq_2(s_neifan_loop[i],s_ru_loop[i],s_pro_loop[i])(x)
      s_eleper_loop[i,] <- t(as.matrix(calculate(s_eleper_fun_2(i,s_ele_loop,s_pro_loop),element_group)(x)))
      element_change_loop[i,] <- t(as.matrix(calculate(element_change_2(i,s_eleper_loop),element_group)(x)))
    }
    if (i > 1 & abs(element_change_loop[i,"TFe"]) < break_value &                  ########## 是否要加abs
        abs(element_change_loop[i,"SiO2"]) < break_value &
        abs(element_change_loop[i,"CaO"]) < break_value ) {
      break
    }
  }
  a <- paste("loop number: ",max(which(s_ru_loop>0)))
  final_index <- max(which(s_ru_loop>0))
  s_ru_final <- s_ru_loop[final_index]
  s_pro_final <- s_pro_loop[final_index]
  s_ele_final <- s_ele_loop[final_index,]  
  s_neifan_final <- s_neifan_loop[final_index]
  s_waifan_final <- s_waifan_loop[final_index]
  s_eleper_final <- s_eleper_loop[final_index,]
  return(list(a,s_ru_final,s_pro_final,s_ele_final,s_neifan_final,s_waifan_final,s_eleper_final))
  #return(list(a,s_ru_loop,s_pro_loop,s_neifan_loop,s_waifan_loop,
  #            s_ele_loop,s_eleper_loop))
}


############################# final output of sinter iteraton ##############################################
#result <- s_fan_interation(x)
#print(result[[1]])
#s_ru_final <- result[[2]]
#s_pro_final <- result[[3]]
#s_ele_final <- result[[4]] 
#s_neifan_final <- result[[5]]
#s_waifan_final <- result[[6]]
#s_eleper_final <- result[[7]]

s_ru_final <- function(x){
  result <- s_fan_interation(x)
  return(result[[2]])
}

s_pro_final <- function(x){
  result <- s_fan_interation(x)
  return(result[[3]])
}

##
s_ele_final <- function(element){
  function(x){
    result <- s_fan_interation(x)
    mat <- result[[4]]
    return(as.numeric(mat[element]))
  }
} 
##
s_neifan_final <- function(x){
  result <- s_fan_interation(x)
  return(result[[5]])
}
##
s_waifan_final <- function(x){
  result <- s_fan_interation(x)
  return(result[[6]])
}

s_neifan_rate <- function(x) {
  test <- s_neifan_final(x)/as.numeric(s_ru_1 %*% x)
  return(as.numeric(test))
}
s_waifan_rate <- function(x){
  test <- s_waifan_final(x)/s_pro_final(x)
  return(as.numeric(test))
}


## Section 5. Continue calculation 
# 4) 烧结考虑元素、质量调整因素
# 加入质量调整因素
s_m_adj <- adj_coef[which(adj_coef$流程=="烧结"),"质量"]
s_pro_final_2 <- function(x){
  return(s_pro_final(x) * s_m_adj)}

########### 加入元素调整因素
index_group <- which(colnames(adj_coef) %in% element_group)
element_adj <- adj_coef[which(adj_coef$流程=="烧结"),min(index_group):max(index_group)]
s_ele_final_2 <- function(element){
  function(x){
    s_ele_final <- s_ele_final(element)(x) * element_adj[element]
    return(as.numeric(s_ele_final))
  }
}
s_eleper_final_2 <- function(element){
  function(x){
    s_eleper_final <- s_ele_final_2(element)(x)/s_pro_final_2(x)
    return(as.numeric(s_eleper_final))
  }
}

####################################



# 1.9) 每吨烧结矿所消耗石灰石用量
s_limestone_per_fun <- function(x){          
  test <- (limestone %*% x)/s_pro_final_2(x)
  return(as.numeric(test))  
}
# 1.10) 烧结入料中各原料占比
s_ru_per_fun <- function(element){          
  function(x){
    test_list <- rep(0,x_count)
    index_test <- which(market$项目==element)
    test_list[index_test] <- 1
    if (market$流程[index_test]=="烧结"){
      if (market$大类[index_test]=="燃料"){
        test <- as.numeric(test_list %*% x)/(s_pro_final_2(x))
      } else {
        test <- (test_list %*% x)/(s_ru_1 %*% x) 
      }
      return(as.numeric(test)) 
    } else {
      message("The input doesn't belong to the Sinter process, pls. double check!") 
    }
  }
}

## 5) blast furnance <2>
# 3.2) 自产铁料：包括自产球团、自产块矿
# a) 入料
# 烧结：生成、内返、外返、库存、外售。烧结入料 = x %*% b_s_ru_coef - b_s_ru_const
b_s_ru_const <- cons_get_data(pro = "烧结", cate = "存量")  # 常数项：每日固定库存烧结矿质量
b_s_ru_fun <- function(x){ 
  test <- s_pro_final_2(x) - s_neifan_final(x) - s_waifan_final(x) - b_s_ru_const
  return(as.numeric(test))  
}
# index: 烧结内返: s_neifan_index; 烧结外返: s_waifan_index; 烧结外售：s_sell_index
# 球团：生成、外返、库存、外售。球团入料 = x %*% p_s_ru_coef - p_s_ru_const
b_p_ru_const <- cons_get_data(pro = "球团", cate = "存量")  # 常数项：每日固定库存球团矿质量
b_p_ru_coef <- p_pro 
b_p_ru_fun <- function(x){ 
  test <- b_p_ru_coef %*% x - p_waifan_eq(x) - b_p_ru_const
  return(as.numeric(test))  
}
# 3.3) 高炉入炉铁料综合：包含外购块矿、烧结、球团+自产烧结、球团
# a) 入料: x*b_tie_ru_coef - b_tie_ru_const
b_tie_ru_const <- b_s_ru_const + b_p_ru_const
b_tie_ru_coef <- b_p_ru_coef + 
  (b_tie_waigou_ru - b_tie_fan)
b_tie_ru_fun <- function(x){ 
  test <- s_pro_final_2(x) - s_neifan_final(x) - s_waifan_final(x) +
    b_tie_ru_coef %*% x -p_waifan_eq(x) - b_tie_ru_const
  return(as.numeric(test))  
}
# b) 铁料相对应各元素占比
# 入炉铁料中各元素的质量
b_tie_ele_fun <- function(element){ 
  function(x){
    test <- b_s_ru_fun(x)/(s_pro_final_2(x)) * s_ele_final_2(element)(x) +
      b_p_ru_fun(x)/(p_pro %*% x) * p_ele_fun(element)(x) + 
      (1 - (b_tie_fan %*% x)/(b_tie_waigou_ru %*% x)) * b_waigou_ele_fun(element)(x)
    return(as.numeric(test))  
  }
}
# 各元素占比（占入炉铁料总量）
b_tie_eleper_fun <- function(element){ 
  function(x){
    test <- b_tie_ele_fun(element)(x)/(b_tie_ru_fun(x))
    return(as.numeric(test))  
  }
}
# c) 入炉铁料质量占比
b_tie_ru_per_fun <- function(element){ 
  function(x){
    if (element == "烧结矿"){
      test <- b_s_ru_fun(x)/ b_tie_ru_fun(x)
    } else if (element == "球团矿"){
      test <- b_p_ru_fun(x)/ b_tie_ru_fun(x)
    } else if (element == "块矿"){
      test <- ((b_tie_waigou_ru - b_tie_fan) %*% x )/ b_tie_ru_fun(x)
    } else {
      test_list <- rep(0,x_count)
      index_test <- which(market$项目==element)
      if (market$流程[index_test]!= "高炉"){ 
        stop("the input doesn't belong to the blast furnace process. Pls. double check!")  }
      test_list[index_test] <- 1
      test <- (test_list  %*% x)/ b_tie_ru_fun(x)
    }
    return(as.numeric(test)) 
  }}

# 3.4) 入炉辅料及燃料
# a) 焦炭
b_tan_ru <- ruliao(pro="高炉",cat="焦炭")
b_tan_ele_fun <- function(element){
  function(x){
    element_percent <- price_elem(pro = "高炉",cate = "焦炭", col = element)
    test <- element_percent %*% x
    return(as.numeric(test))  
  }
}
# b) 煤
b_mei_ru <- ruliao(pro="高炉",cat="喷吹煤")
b_mei_ele_fun <- function(element){
  function(x){
    element_percent <- price_elem(pro = "高炉",cate = "喷吹煤", col = element)
    test <- element_percent %*% x
    return(as.numeric(test))  
  }
}
# c) 辅料
b_fu_ru <- ruliao(pro="高炉",cat="辅料")
b_fu_ele_fun <- function(element){
  function(x){
    element_percent <- price_elem(pro = "高炉",cate = "辅料", col = element)
    test <- element_percent %*% x
    return(as.numeric(test))  
  }
}
# 3.5) 高炉总入料
# a) 总入料量
b_ru_fun <- function(x){ 
  test <- b_tie_ru_fun(x) + as.numeric(b_tan_ru %*% x) + 
    as.numeric(b_mei_ru %*% x) + as.numeric(b_fu_ru %*% x)
  return(as.numeric(test))  
}
# b) 化学元素
# b.1) 各化学元素入炉总质量, 考虑铁燃辅料
b_ru_ele_fun <- function(element){ 
  function(x){
    test <- b_tie_ele_fun(element)(x) + b_tan_ele_fun(element)(x) +
      b_mei_ele_fun(element)(x) + b_fu_ele_fun(element)(x)
    return(as.numeric(test))  
  }
}
# b.2) 各化学元素质量占比（占铁燃辅总入料量）
b_ru_eleper_fun <- function(element){ 
  function(x){
    test <- b_ru_ele_fun(element)(x)/(b_ru_fun(x))
    return(as.numeric(test))  
  }
}
# c) 燃辅料入料占比
b_ranfu_ru_per_fun <- function(element){
  function(x){
    test_list <- rep(0,x_count)
    index_test <- which(market$项目==element)
    test_list[index_test] <- 1
    if (market$流程[index_test] == "高炉" & market$大类[index_test] == "焦炭"){
      test <- (test_list %*% x)/ (b_tieshui_fun(x))
    } else if (market$流程[index_test] == "高炉" & market$大类[index_test] == "喷吹煤"){
      test <- (test_list %*% x)/ (b_tieshui_fun(x))
    } else if (market$流程[index_test] == "高炉" & market$大类[index_test] == "辅料"){
      test <- (test_list %*% x)/ b_ru_fun(x)
    } else {
      message("Pls. double check the input!")
    }
    return(as.numeric(test)) 
  }}
# 3.6) 生成物-铁水
# a) 铁水调整系数
b_tieshui_coef_fun <- function(element){
  n1 <- which(adj_coef$项目=="各元素在高炉反应后的保留率")
  n2 <- which(adj_coef$项目=="进铁水比例")
  n3 <- which(adj_coef$项目=="铁水中元素离子质量系数")
  test <- adj_coef[n1,element] * adj_coef[n2,element] *
    adj_coef[n3,element]
  return(test)
}
# b) 铁水产量
shoude <- form_coef[which(form_coef$项目=="铁元素收得率"),"系数"]
hanliang  <- form_coef[which(form_coef$项目=="铁水中铁元素占比"),"系数"]
b_tieshui_fun <- function(x){
  test <- b_ru_ele_fun("TFe")(x) * b_tieshui_coef_fun("TFe") * shoude / hanliang
  return(test)
}
# c) 铁水中各元素成分占比
b_tieshui_ele_per_fun <- function(element){ 
  function(x){
    test <- b_ru_ele_fun(element)(x) * b_tieshui_coef_fun(element)/ b_tieshui_fun(x)
    return(as.numeric(test))  
  }
}
# 3.7) 生成物-炉渣
# a) 炉渣调整系数
b_zha_coef_fun <- function(element){
  n1 <- which(adj_coef$项目=="各元素在高炉反应后的保留率")
  n2 <- which(adj_coef$项目=="进渣比例")
  test <- adj_coef[n1,element] * adj_coef[n2,element]
  return(test)
}
# b) 炉渣总量
zha_mass_coef <- adj_coef[which(adj_coef$项目=="进渣比例"),"质量"]
b_zha_fun <- function(x){
  start_index <- which(colnames(market)=="price.干基.")
  end_index <- which(colnames(market)=="烧损LOI")
  element_group <- c(colnames(market)[(start_index+1):(end_index-1)])
  zha_mass_vector <- rep(0,length(element_group)) 
  for (i in (1:length(zha_mass_vector))){
    zha_mass_vector[i] <- b_ru_ele_fun(element_group[i])(x)*b_zha_coef_fun(element_group[i])
  }
  return(sum(as.numeric(zha_mass_vector)))
}
# d) 炉渣中各元素成分占比
b_zha_ele_per_fun <- function(element){ 
  function(x){
    test <- b_ru_ele_fun(element)(x) * b_zha_coef_fun(element)/ b_zha_fun(x)
    return(as.numeric(test))  
  }
}
# 3.8) 价格
b_price_tieranfu <- price_elem(pro="高炉",cate="ALL",col="price.干基.")
b_fixed_cost <- cons_get_data(pro = "高炉", cate = "日均成本")
b_waifan <- b_tie_fan
#b_waifan[s_waifan_index] <- 1
#b_waifan[p_waifan_index] <- 1
b_price_fan <- constant[which(constant$项目=="返矿单价"),"数值"] * b_waifan

## Section 6. Optimization
## 1) target function:  -收入 + 成本
cost_minus_profit <- function(x){
  price_tie <- constant[which(constant$项目=="铁水市场价"),"数值"]
  #price_s <- constant[which(constant$项目=="烧结矿市场价"),"数值"]
  #price_p <- constant[which(constant$项目=="球团矿市场价"),"数值"]
  q_tie <- b_tieshui_fun(x)
  #q_s <- rep(0,x_count)
  #q_s[s_sell_index] <- 1
  #q_p <- rep(0,x_count)
  #q_p[p_sell_index] <- 1
  revenue <- q_tie * price_tie #+ as.numeric((q_s*price_s) %*% x) + as.numeric((q_p * price_p) %*% x)
  cost <- s_price %*% x + s_fixed_cost +
    p_price %*% x + p_fixed_cost + p_ran_cost %*% x + 
    b_price_tieranfu %*% x + b_fixed_cost - b_price_fan %*% x
  profit <- cost - revenue
  return(as.numeric(profit))
}
## 3) 不等式限制
# 定位
s_tie_cons_index <- max(which(cons$流程=="烧结" & cons$大类 == "铁料"))
s_ran_cons_index <- max(which(cons$流程=="烧结" & cons$大类 == "燃料"))
s_fu_cons_index <- max(which(cons$流程=="烧结" & cons$大类 == "辅料"))
s_ind_index <- max(which(cons$流程=="烧结" & cons$大类 == "关键指标"))
s_prod_index <- max(which(cons$流程=="烧结" & cons$大类 == "产量"))
p_tie_cons_index <- max(which(cons$流程=="球团" & cons$大类 == "铁料"))
p_fu_cons_index <- max(which(cons$流程=="球团" & cons$大类 == "辅料"))
p_prod_index <- max(which(cons$流程=="球团" & cons$大类 == "产量"))
b_tie_cons_index <- max(which(cons$流程=="高炉" & cons$大类 == "铁料"))
b_mei_cons_index <- max(which(cons$流程=="高炉" & cons$大类 == "喷吹煤"))
b_tan_cons_index <- max(which(cons$流程=="高炉" & cons$大类 == "焦炭"))
b_fu_cons_index <- max(which(cons$流程=="高炉" & cons$大类 == "辅料"))
b_rulu_ind_index <- max(which(cons$流程=="高炉" & cons$大类 == "综合指标"))
b_ru_ttl_index <- max(which(cons$流程=="高炉" & cons$大类 == "总入料"))
b_tieshui_ind_index <- max(which(cons$流程=="高炉" & cons$大类 == "铁水指标"))
b_zha_ind_index <- max(which(cons$流程=="高炉" & cons$大类 == "炉渣指标"))
s_neifan_cons_index <- max(which(cons$流程=="烧结" & cons$大类 == "内返"))
s_waifan_cons_index <- max(which(cons$流程=="烧结" & cons$大类 == "外返"))
p_waifan_cons_index <- max(which(cons$流程=="球团" & cons$大类 == "外返"))

cons_lb <- function(nrow){           ## 注意cons的列名为上下限而不是lb/ub
  if (cons[nrow,"单位"] == "%"){
    lb <- cons[nrow,"下限"]/100
  } else {
    lb <- cons[nrow,"下限"]
  }
  return(lb)
}
cons_ub <- function(nrow){
  if (cons[nrow,"单位"] == "%"){
    ub <- cons[nrow,"上限"]/100
  } else {
    ub <- cons[nrow,"上限"]
  }
  return(ub)
}
eval_ineq_lb <- function(x){
  h <- rep(0,nrow(cons))   ## 考虑上限和下限怎么处理
  for (i in (1:length(h))){
    if (i <= s_fu_cons_index){
      target <- cons$项目[i]
      #print(target)
      h[i] <- cons_lb(i)- s_ru_per_fun(target)(x)
    } else if (i <= s_ind_index) {
      target <- cons$项目[i]
      #print(target)  ####Cao的O小写会导致报错
      ifelse(target=="二维碱度",h[i] <- cons_lb(i) - s_eleper_final_2("CaO")(x)/s_eleper_final_2("SiO2")(x),
             ifelse(target=="四维碱度",h[i] <- cons_lb(i) - (s_eleper_final_2("MgO")(x)+s_eleper_final_2("CaO")(x))/
                      (s_eleper_final_2("SiO2")(x)+s_eleper_final_2("Al2O3")(x)),
                    h[i] <- cons_lb(i) -s_eleper_final_2(target)(x) )) 
    } else if (i <= s_prod_index) {
      #print("s_prod")
      h[i] <- cons_lb(i) - as.numeric(s_pro_final_2(x))  
    } else if (i <= p_fu_cons_index) {
      target <- cons$项目[i]
      #print(target)
      h[i] <- cons_lb(i) - p_ru_per_fun(target)(x)
    } else if (i <= p_prod_index) {
      h[i] <- cons_lb(i) - as.numeric(p_pro %*% x)
    } else if (i <= b_tie_cons_index) {
      target <- cons$项目[i]
      #print(target)
      h[i] <- cons_lb(i) - b_tie_ru_per_fun(target)(x)
    } else if (i <= b_fu_cons_index) {
      target <- cons$项目[i]
      #print(target)
      h[i] <- cons_lb(i) - b_ranfu_ru_per_fun(target)(x) 
    } else if (i <= b_rulu_ind_index) {
      target <- cons$项目[i]
      #print(target)
      ifelse(target=="综合入炉品位", h[i] <- cons_lb(i) - b_tie_eleper_fun("TFe")(x)
             , h[i] <- cons_lb(i) - b_tie_eleper_fun(target)(x))
    } else if (i <= b_ru_ttl_index) {
      h[i] <- cons_lb(i) - b_ru_fun(x)
    } else if (i <= b_tieshui_ind_index) {
      target <- cons$项目[i]
      h[i] <- cons_lb(i) - b_tieshui_ele_per_fun(target)(x)
    } else if (i <= b_zha_ind_index) {
      target <- cons$项目[i]
      ifelse(target=="二维碱度",h[i] <- cons_lb(i) - b_zha_ele_per_fun("CaO")(x)/b_zha_ele_per_fun("SiO2")(x),
             ifelse(target=="镁铝比",h[i] <- cons_lb(i) - b_zha_ele_per_fun("MgO")(x)/b_zha_ele_per_fun("Al2O3")(x),
                    h[i] <- cons_lb(i) - b_zha_ele_per_fun(target)(x) )) 
    } else if (i <= s_neifan_cons_index) {
      h[i] <- cons_lb(i) - s_neifan_rate(x)
    } else if (i <= s_waifan_cons_index) {
      h[i] <- cons_lb(i) - s_waifan_rate(x)
    } else if (i <= p_waifan_cons_index) {
      h[i] <- cons_lb(i) - p_waifan_rate(x)
    } 
  }
  return(as.numeric(h))
}
eval_ineq_ub <- function(x){
  h <- rep(0,nrow(cons))   ## 考虑上限和下限怎么处理
  for (i in (1:length(h))){
    if (i <= s_fu_cons_index){
      target <- cons$项目[i]
      h[i] <- cons_ub(i)- s_ru_per_fun(target)(x)
    } else if (i <= s_ind_index) {
      target <- cons$项目[i]
      ifelse(target=="二维碱度",h[i] <- cons_ub(i) - s_eleper_final_2("CaO")(x)/s_eleper_final_2("SiO2")(x),
             ifelse(target=="四维碱度",h[i] <- cons_ub(i) - (s_eleper_final_2("MgO")(x)+s_eleper_final_2("CaO")(x))/
                      (s_eleper_final_2("SiO2")(x)+s_eleper_final_2("Al2O3")(x)),
                    h[i] <- cons_ub(i) -s_eleper_final_2(target)(x) )) 
    } else if (i <= s_prod_index) {
      h[i] <- cons_ub(i) - as.numeric(s_pro_final_2(x))  
    } else if (i <= p_fu_cons_index) {
      target <- cons$项目[i]
      h[i] <- cons_ub(i) - p_ru_per_fun(target)(x)
    } else if (i <= p_prod_index) {
      h[i] <- cons_ub(i) - as.numeric(p_pro %*% x)
    } else if (i <= b_tie_cons_index) {
      target <- cons$项目[i]
      h[i] <- cons_ub(i) - b_tie_ru_per_fun(target)(x)
    } else if (i <= b_fu_cons_index) {
      target <- cons$项目[i]
      h[i] <- cons_ub(i) - b_ranfu_ru_per_fun(target)(x)
    } else if (i <= b_rulu_ind_index) {
      target <- cons$项目[i]
      ifelse(target=="综合入炉品位", h[i] <- cons_ub(i) - b_tie_eleper_fun("TFe")(x)
             , h[i] <- cons_ub(i) - b_tie_eleper_fun(target)(x))
    } else if (i <= b_ru_ttl_index) {
      h[i] <- cons_ub(i) - b_ru_fun(x)
    } else if (i <= b_tieshui_ind_index) {
      target <- cons$项目[i]
      h[i] <- cons_ub(i) - b_tieshui_ele_per_fun(target)(x)
    } else if (i <= b_zha_ind_index) {
      target <- cons$项目[i]
      ifelse(target=="二维碱度",h[i] <- cons_ub(i) - b_zha_ele_per_fun("CaO")(x)/b_zha_ele_per_fun("SiO2")(x),
             ifelse(target=="镁铝比",h[i] <- cons_ub(i) - b_zha_ele_per_fun("MgO")(x)/b_zha_ele_per_fun("Al2O3")(x),
                    h[i] <- cons_ub(i) - b_zha_ele_per_fun(target)(x) )) 
    } else if (i <= s_neifan_cons_index) {
      h[i] <- cons_ub(i) - s_neifan_rate(x)
    } else if (i <= s_waifan_cons_index) {
      h[i] <- cons_ub(i) - s_waifan_rate(x)
    } else if (i <= p_waifan_cons_index) {
      h[i] <- cons_ub(i) - p_waifan_rate(x)
    }}
  return(-(as.numeric(h)))
}

eval_ineq <- function(x){
  o <- rep(0,nrow(cons)*2)
  o <- append(eval_ineq_lb(x),eval_ineq_ub(x))
  return(o)
}

###########################################################################################################################
###########################################################################################################################
# initial values
x0 <- x_defi[,4]  # x0 <- runif(4,min = 0,max = 100000) | x0/lower/upper <- rep(3,25)

# lower and upper bounds of control
zero_item <- which(cons$下限 == 0 & cons$上限 == 0)
#fix_item <- which(cons$lb == cons$ub & cons$lb != 0)
zero_item_index <- which(market$项目 %in% cons$项目[zero_item])
#fix_item_index <-  which(market$项目 %in% cons$项目[fix_item])
#fix_value <- cons$lb[fix_item]
message("Total number of zero input elements ",length(zero_item))
#message("Total number of fixed input elements ",length(fix_item))
# 下限
lb_test <- x_defi[,5]
# names(lb) <- raw_material
lb_test[zero_item_index] <- 0
# lb[fix_item_index] <- as.numeric(fix_value)
# 上限
ub_test <- x_defi[,6]
#names(ub) <- raw_material
ub_test[zero_item_index] <- 0
# ub[fix_item_index] <- as.numeric(fix_value)

local_opts <- list("algorithm" = "NLOPT_LN_COBYLA",  
                   "xtol_rel" = 1.0e-1)
# NLOPT_LN_COBYLA | NLOPT_LN_NEWUOA | NLOPT_LN_BOBYQA

opts <- list("algorithm" = "NLOPT_LN_AUGLAG",
             "xtol_rel" = 1.0e-1,
             "print_level" = 3,
             # "check_derivatives" = TRUE,
             # "check_derivatives_print" = "all",
             "maxeval" = 500,
             "local_opts" = local_opts
)

res <- nloptr( x0 = x0,
               eval_f = cost_minus_profit,
               lb = lb_test,
               ub = ub_test,
               eval_g_ineq = eval_ineq,
               eval_g_eq = NULL, #eval_eq,
               opts = opts)

x_result <- res$solution
x_ojective <- res$objective
print(res)

# 
res <- auglag(x0, cost_minus_profit,lower=lb_test,upper=ub_test,
              hin = eval_ineq, heq = NULL,
              localsolver = c("COBYLA"),localtol = 1e-6,
              control = list("xtol_rel" = 1.0e-7,"maxeval" = 2000))






















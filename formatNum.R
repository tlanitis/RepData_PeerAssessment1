formatNum<- function (number,decimals) {
        format(round(as.numeric(number),decimals),nsmall=decimals,big.mark=",")
}
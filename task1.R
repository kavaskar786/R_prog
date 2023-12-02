#1.
sales_data=c(45,60,35,75,80,62,48,53,69,72,40,55)

#2
sum1=sum(sales_data)
print(sum1)

#3
avg=sum1/12
print(avg)

#4
highest_sales=max(sales_data)
print(highest_sales)
highest_month=which.max(sales_data)
highest_month

#4
lowest_sales=min(sales_data)
print(lowest_sales)
lowest_month=which.min(sales_data)
lowest_month

#5
incre=(sales_data[c(3)]*(10/100))+sales_data[c(3)]
print(incre)

#6
sorted_sales=sort(sales_data)
print(sorted_sales)

#7
rev_sorted_sales=sort(sales_data,decreasing = T)
print(rev_sorted_sales)

#8
print(median(sales_data))




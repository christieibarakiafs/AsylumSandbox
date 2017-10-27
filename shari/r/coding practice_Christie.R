#Q1: mean: takes an array of numbers, returns the mean
compute_mean = function(vector){
  my_mean = sum(vector)/length(vector)
  return(my_mean)
}

x = c(23,5,82,100,93)
compute_mean(x)

#Q2:isPrime: takes a number, returns Boolean indicating whether number is prime
isPrime = function(num){
  remaindar = c() #create a placeholder
  if(num>1){
    for (i in seq_len(num)[c(-1,-num)]){ #remove 1 and the number itself
      remaindar[i] = num%%seq_len(num)[i]
    }
    remaindar = remaindar[!is.na(remaindar)] #remove NA (in the first index due to the removal of 1)
    if(all(remaindar!=0)){
      return(1)
    }else{
      return(0)
    }
  }else if(num==1){
      return(1) #if num
  }else{
      print("the number is less than 1")
  }
}

isPrime(11) #if it's prime, it's a 1


#Q3:squareRoot: takes number, returns square root
my_squareRoot = function(num){
  my_seq = seq(1,num/2,by=0.000001)
  for(i in 1:length(my_seq)){
    square = my_seq[i]*my_seq[i]
    if(square>=num){
      break
    }
  }
  root = my_seq[i]
  print(root)
}

my_squareRoot(10)

#Q4:greatestCommonFactor: takes two numbers, returns greatest common factor (bonus: using recursion)
greatestCommonFactor=function(num1,num2){
  my_nums = c(num1,num2)
  smaller_num = my_nums[which.min(my_nums)] #get the smaller number to narrow down the common factor possibilities
  smaller_num_seq = seq(smaller_num)
  num1_remaindar = c()
  num2_remaindar = c()
  for(i in 1:smaller_num){
    num1_remaindar[i] = num1%%smaller_num_seq[i]
    num2_remaindar[i] = num2%%smaller_num_seq[i]
  }
  num1_seq = seq(num1)
  num2_seq = seq(num2)
  num1_divisor = num1_seq[which(num1_remaindar==0)]
  num2_divisor = num2_seq[which(num2_remaindar==0)]
  gcd = max(intersect(num1_divisor,num2_divisor))
  return(gcd)
}

greatestCommonFactor(27,9)


















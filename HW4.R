# Question 1 :�N�Ұ󤤪��ۭq�ƧǨ�ƥ[�J decreasing = ���Ѽơ]�w�]�� FALSE�^���ϥΪ̥i�H���w���W�λ���Ƨ�
exchange.sort.asc <- function(input_vector,decreasing=FALSE) {
  if(decreasing==TRUE){
    for (i in 1:(length(input_vector) - 1)) {
      for (j in (i + 1):length(input_vector)) {
        if (input_vector[i] > input_vector[j]) {
          temp <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- temp
        }
      }
    }
    return(input_vector)
  }else{for (i in 1:(length(input_vector) - 1)) {
    for (j in (i + 1):length(input_vector)) {
      if (input_vector[i] < input_vector[j]) {
        temp <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- temp
      }
    }
  }
    return(input_vector)
  }
}



# ���X�@���H���V�q
unsorted_vector <- round(runif(10) * 100)
# ����H���V�q�����W�Ƨ�

exchange.sort.asc(unsorted_vector,decreasing= T)
exchange.sort.asc(unsorted_vector,decreasing= F)

#------------------------------------------------------------------
# Question 2 : �ۭq�p��˥��зǮt�����

# Section 2: self-defined functions
my.sd <- function(input_vector) {
  sum <- 0
  count <- 0
  for (i in input_vector) {
    sum<- sum + (i - mean(input_vector))^2
    count <- count + 1
  }
  return(sqrt(sum/(count-1)))
}

# Section 3: inputs and parameters
my_vector <- 1:50


# Section 4: function calls
my.sd(my_vector)

sd(my_vector) #check with sd()
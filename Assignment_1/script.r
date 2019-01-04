# Task 2. read data
cpu <- read.csv(file="cpu.csv", header=TRUE, sep=";")
mem <- read.csv(file="mem.csv", header=TRUE, sep=";")
energy <- read.csv(file="energy.csv", header=FALSE, sep=";") # why do we need this?
net <- read.csv(file="net.csv", header=TRUE, sep=";")

# Task 3. convert data frame to matrix
cpu = data.matrix(cpu) 
net = data.matrix(net) 
mem = data.matrix(mem)
# indexing: cpu[ index of sample (1:179), index of VM (1:36) ]

# Task 4. create covariance and correlation matrices from data (cpu, mem, net)
cov_cpu = cov(cpu)
cov_mem = cov(mem)
cov_net = cov(net)

cor_cpu = cor(cpu)
cor_mem = cor(mem)
cor_net = cor(net)

# Task 5. create standardized matrix for each normal matrix
# (self-written) function for generating a standardized matrix 
generate_stdm <- function(mat) {
    stdm = matrix(, nrow=179, ncol=36)
    # iterate over each VM
    for (j in 1:36) {
        # calculate standard deviation and mean for this VM
        m = mean(mat[1:179,j])
        std = sd(mat[1:179,j])

        # set the value at index i,j in the standardized matrix
        for (i in 1:179) {
            stdm[i,j] = (mat[i,j] - m)/std
        }
    }
    return(stdm)
}

# standardized matrix from built-in function
stdm_cpu = scale(cpu)
stdm_mem = scale(mem)
stdm_net = scale(net)

gstdm_cpu = generate_stdm(cpu)
gstdm_mem = generate_stdm(mem)
gstdm_net = generate_stdm(net)

# comparing "scale" and self-written function
print( gstdm_cpu[1,4] )
print( stdm_cpu[1,4] )
print( gstdm_mem[10,9] )
print( stdm_mem[10,9] )
print( gstdm_net[150,19] )
print( stdm_net[150,19] )
# ...they are equivalent

# Task 6. Generate a 3d-array from the three row matrices (vm vs samples vs resources)
ar <- array(rep(NaN, 36*179*3), c( 36, 179, 3 ))
for (i in 1:36) {
    for (j in 1:179) {
        ar[i,j,1] = cpu[j,i]
        ar[i,j,2] = mem[j,i]
        ar[i,j,3] = net[j,i]
    } 
}

# check if result is correct
print(ar[1,1,1])
print(cpu[1,1])
print(ar[1,5,2])
print(mem[5,1])

# Task 7. Plot the density functions of the CPU utilisation for the following virtual machines
# 7.a. The five virtual machines whose average CPU utilisation is the highest
means = apply(ar[1:36,1:179,1], 1, mean)
ids = order(means, decreasing=TRUE)
# Top 5. with highest average CPU utilisation @ ids[1], ids[2],...ids[5]
# plot(density(ar[ids[1],1:179, 1])) ...

# 7.b. The five virtual machines whose average CPU utilisation is the highest
variances = apply( ar[1:36,1:179,1], 1, var)
ids = order(variances, decreasing=TRUE)
# Top 5. with highest variance @ ids[1], ids[2],...ids[5]
# plot(density(ar[ids[1],1:179, 1])) ...


# Task 8. Is there a correlation between the utilisation of CPU and MEM?
correlations = c()
for (i in 1:36) {
  # avoid divison by 0 -> no NA's in calculation
  if (var(ar[i,1:179,2]) != 0 && var(ar[i,1:179,1]) != 0) {
    cor_ = cor(ar[i,1:179,1], ar[i,1:179,2])
    correlations <- c(correlations, cor_)
    print(cor_)
  }
}

# show results for Task 8.
plot(density(correlations))
print(mean(correlations))
print(var(correlations))

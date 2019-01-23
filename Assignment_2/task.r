# 2. read data
bike <- read.csv(file="Schreibtisch/WiSe5/Prediction_And_Estimation_Techniques/Assignment_2/bike_fs_512_manual.csv", header=TRUE, sep=",")

# 4. Slice into heartbeats, store result as a matrix (hearbeat vs. samples)
bike = data.matrix(bike)

# important
# LA = Left Arm
# LL = Left Leg
# RA, RL vice versa

# 97328 vs 22
# dim(bike)

# ECG_LL_RA_24BIT_CAL
ll_ra = bike[1:97328,12]
# ECG_LL_LA_24BIT_CAL
ll_la = bike[1:97328,11]
# ECG_LA_RA_24BIT_CAL
la_ra = bike[1:97328,10]

plot_heartbeats <- function( heartbeats ) {
  colors = c("green", "yellow", "red", "black", "blue", "orange")
  
  for (i in 1:length(heartbeats)) {
    lines(unlist(heartbeats[i]),type="l",col=colors[(i %% length(colors)) + 1])
  }
}

get_heartbeats <- function( target, diff_threshold, width, idx_threshold  ) {
  target_tmp = target[1:length(target)]
  # global maximum
  g_max = max(target)
  ret = list()
  
  while (1) {
    minimum = min(target_tmp)
    # loop until every part of the array is checked
    if (minimum == g_max)
      return(ret)
    idx = match(minimum, target_tmp)
    
    l_idx = max(idx-width,1)
    r_idx = min(idx+width,length(target))
  
    # for extrapolation.
    l = rep( target[ l_idx ], max( width-idx+1,0 ) ) 
    r = rep( target[ r_idx ], max( idx+width-length(target),0 ))
    # select all elements and extrapolate if necessary.
    tmp = c(l, target[l_idx : r_idx], r)  
    target_tmp[l_idx : r_idx] = g_max
    
    
    # local maximum
    l_max = max(tmp)
    # check if difference is great enough,
    # minimum and maximum are close together
    # and minimum comes after maximum
    # if yes, this sample is considered as a heartbeat.
    diff = l_max - minimum
    diff_idx = match(minimum, tmp) - match(l_max, tmp)
    
    if (diff > diff_threshold && diff_idx > idx_threshold[1] && diff_idx < idx_threshold[2])
        ret = c(ret, list(tmp))
    
  }
}


hbts_la_ra = get_heartbeats(la_ra, 0, 150, c(5,40))
hbts_ll_la = get_heartbeats(ll_la, 1.0, 150, c(5,40))
hbts_ll_ra = get_heartbeats(ll_ra, 1.0, 150, c(5,40))

print(length(hbts_la_ra))
print(length(hbts_ll_la))
print(length(hbts_ll_ra))

# plot(c(), xlim=c(1,300), ylim=c(-15,10))
# plot_heartbeats(hbts_la_ra)
# plot_heartbeats(hbts_ll_la)
# plot_heartbeats(hbts_ll_ra)

# determine the regularity of the heartbeats. 
# plot(density(unlist(hbts_ll_ra[50])), ylim=c(0,2), col="green")
# lines(density(unlist(hbts_ll_ra[100])), col="red")
# lines(density(unlist(hbts_ll_ra[150])), col="orange")
# lines(density(unlist(hbts_ll_ra[200])), col="black")

# plot(density(unlist(hbts_ll_la[50])), ylim=c(0,3), col="green")
# lines(density(unlist(hbts_ll_la[100])), col="red")
# lines(density(unlist(hbts_ll_la[150])), col="orange")
# lines(density(unlist(hbts_ll_la[200])), col="black")

hbts_scaled_ll_la = get_heartbeats(scale(ll_la), 1.0, 150, c(5,40))
hbts_scaled_ll_ra = get_heartbeats(scale(ll_ra), 1.0, 150, c(5,40))
hbts_scaled_la_ra = get_heartbeats(scale(la_ra), 1.0, 150, c(5,40))
# plot_heartbeats(hbts_scaled_la_ra)
# plot_heartbeats(hbts_scaled_ll_la)
# plot_heartbeats(hbts_scaled_ll_ra)

# select 5 samples
plot( density(unlist(hbts_scaled_la_ra[1])), xlim=c(-4,4), ylim=c(0,3), col="green" )
lines( density(unlist(hbts_scaled_la_ra[50])), col="red" )
lines( density(unlist(hbts_scaled_la_ra[100])), col="orange" )
lines( density(unlist(hbts_scaled_la_ra[150])), col="black" )
lines( density(unlist(hbts_scaled_la_ra[140])), col="grey" )

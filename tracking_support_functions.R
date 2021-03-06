parseTrajectory <- function(trajectory, relative = FALSE, center=0){
  # Takes a python-style list of numbers, and returns a vector
  # Not usually called directly, but via extractTrajectory
  # ARGS: trajectory - a string of the form "[n, o, p, ..., q, r, s]"
  # RETURNS: the vector = c(n,o,p,...,q,r,s)
  
  unlist(strsplit(trajectory, split=",")) -> parsed.traj
  # for reasons I don't understand, I have not been able to successfully combine \\[ with \\] in a regex
  # thus there are two statements
  gsub("\\]", "", parsed.traj) -> parsed.traj
  gsub("[\\[, ]","", parsed.traj) -> parsed.traj
  
  if(relative){
    as.numeric(parsed.traj) -> parsed.traj
    parsed.traj - center -> parsed.traj
    
  }
  
  return(as.numeric(parsed.traj))
  
}

normalizeTrajectory <- function(traj, center.x = 640, center.y = 800, pic_response, interpretation, current.item){
# Not usually called directly, but via extractTrajectory
  
  traj$x <- traj$x - center.x
  # On Android positive y is toward bottom of screen
  # Here y coords are tranformed so that a positive value is 'up' relative to starting point
  traj$y <- -(traj$y - center.y)  
  #print(pic_response)
  #print(current.item)
  #print(head.pic[current.item])
  # Reflect x coordinates so that the 'reflexive' side is always to the eft
  if((interpretation=="reflexive" & pic_response=="right") | (interpretation=="disjoint" & pic_response=="left")){
    #print("Reverse it!")
    traj$x <- -traj$x
  }
  # TODO:normalize time
  
  # Decorate with useful quantities
  traj$length <- length(traj$x)
  traj$final.x <- traj$x[traj$length]
  traj$final.y <- traj$y[traj$length]
  
  attr(traj, "normalized") <- "yes"
  
  return(traj)
}

countReversals <-function(traj.vec){
  
  # direction
  sign(diff(traj.vec)) -> direction
  which(direction==0) -> stalls
  
  if(length(stalls) > 0){
    direction[direction!=0] -> direction
  }
  
  # changing direction of change
  diff(direction) -> change.direction
  
  # count changes; /2 because diff(1,-1) = 2
  sum(abs(change.direction/2)) -> reversals
  
  return(list(reversals = reversals, stalls = length(stalls)))
}


extractTrajectory <- function(trial.df){
  
  parseTrajectory(trial.df$xTrajectory) -> x.trajectory
  parseTrajectory(trial.df$yTrajectory) -> y.trajectory
  parseTrajectory(trial.df$tTrajectory) -> t.trajectory

  t.trajectory <- t.trajectory - trial.df$duration
  
  trajectory <- list(x = x.trajectory, y=y.trajectory, t=t.trajectory, length=length(t.trajectory), raw=
                       list(x=x.trajectory, y=y.trajectory, t=t.trajectory), dist=NA)

  attr(trajectory, "description") <- "Touch tracking trajectory"
  trajectory$raw <- trajectory[1:3]
  trajectory <- normalizeTrajectory(trajectory,  pic_response = trial.df$pic_response, 
                                    interpretation = trial.df$interpretation, 
                                    current.item = trial.df$current_item)
 
  countReversals(trajectory$x)$reversals -> trajectory$x.reversals
  countReversals(trajectory$x)$stalls -> trajectory$x.stalls
  
  tlen <- length(trajectory$x)
  trajectory$dist <- sqrt((trajectory$x[tlen]- trajectory$x)^2+ (trajectory$y[tlen] - trajectory$y)^2)

  return(trajectory)
}


###

plotTrajectory.xy <- function(data, index, superimpose=FALSE, connected=TRUE){
  
  extractTrajectory(data[index,]) -> track
  track$x -> x
  track$y -> y
  
  par.pty <- "p"
  par.lty <- "dashed"
  if(connected){ 
    par.pty <- "b"
  }
  
  if(sign(x[length(x)])==-1){ 
    par.col <- "blue"
    par.side <- 2
  } else { 
    par.col <- "red"
    par.side <- 4
  }
  
  if(superimpose){
    points(x, y, type = par.pty, col=par.col)
    } 
  else {
    plot(x, y, type = par.pty,
         xlim=c(-640, 640),
         ylim=c(-100, 700),
         lty = par.lty, col=par.col)
         
  }
  rug(x[length(x)], col=par.col)
  rug(y[length(y)], side=par.side, col=par.col)
  
 # segments(-20, 0, 20, 0, col="red", lwd=3)
 #segments(0, -20, 0, 20, col="red", lwd=3)

  abline(h=0, v=0, col="red", lty="dotted")
}

###
###
###
plotTrajectory.intime <- function(data, index, superimpose=FALSE){
  
  parseTrajectory(data$xTrajectory[index], relative=TRUE, center=640) -> x
  parseTrajectory(data$yTrajectory[index], relative=TRUE, center=700) -> y
  parseTrajectory(data$tTrajectory[index]) -> t
  
  if(superimpose){
    lines(t,x, type="b", lty="dotted")}
  else{plot(t, x, pch='x',xlim=c(4000,8000), lty="dotted", type='b')}
 # points(t, -y, pch='y')
  
}

#average.reversals <- round(with(experiment.df, tapply(x.reversals, list(rc.type, verb.type, interpreted.as.agent), mean)),2)

xyt.plot <- function(trial, superimpose=FALSE){
  
  extractTrajectory(trial) -> track

  par(mar=c(3,3,1,1))
 
  track$x <- c(0, track$x)
  track$y <- c(0, track$y)
  track$t <- c(min(track$t) - 70, track$t)
  
  
  if(!superimpose){
    split.screen(c(1,2))
    
    screen(1)
    plot(track$x, track$y, type='b', ylim=c(-100,700), xlim=c(-640,640))
    screen(2)
    plot(track$t, track$x, xlim=c(2000,5000), type='b')
  } else {
    screen(1)
    points(track$x, track$y)
    screen(2)
    points(track$t, track$x)
  }
  return(track)
}


average.trajectory <- function(trial.df, index.vec, window=100){
  
  extractTrajectory(trial.df[index.vec[1],]) -> first.track
  track.df <- data.frame(x = first.track$x, y = first.track$y, t = first.track$t, index = 1)
  
  for(i in 2:length(index.vec)){
    extractTrajectory(trial.df[index.vec[i],]) -> next.track
    next.df <- data.frame(x = next.track$x, y = next.track$y, t = next.track$t, index=i)
    merge(track.df, next.df, all=TRUE) -> track.df
  }
  
  min(track.df$t) -> left.edge
  max(track.df$t) -> right.edge
  
  floor(left.edge/window)*window -> left.edge
  ceiling(right.edge/window)*window -> right.edge
  
  seq(left.edge, right.edge, by = window) -> breaks
  
  track.df$bin <- cut(track.df$t, breaks)
  
  print(left.edge)
  print(right.edge)
  return(track.df)
  
}
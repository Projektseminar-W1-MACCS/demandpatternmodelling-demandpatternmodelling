#####
# Auxilary Function
#####


if VOL_VAR == -1
VOL_VAR_MIN = 0.5; 
VOL_VAR_MAX = 1.5;
VOL_VAR = VOL_VAR_MIN + (VOL_VAR_MAX-VOL_VAR_MIN).*rand(1,1);
else
  end 

#Power function(use to determine power required to keep a vehicle moving 
#at x speed) in which parameters are:
#' @param cdrag dragging coefficient set at=0.3
#' @param crolling coefficient set at =0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A front of vehicle area (m2)
#' @param g gravity acceleration (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

autopower = function(V, m, A, cdrag=0.3, crolling=0.015,pair=1.2,g=9.8) {
   P = crolling*m*g*V + 1/2*A*pair*cdrag*V**3
  return(P)
}




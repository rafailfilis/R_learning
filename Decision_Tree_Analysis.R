probs <- list(

  # option A ==> surgery
  prob_surgery_succ = 0.75,
  prob_surgery_fail = 0.25,
  # if surgery fails  ==> baseline treatment for relapse
  prob_treatment_succ = 0.65,
  prob_treatment_fail = 0.35,

  # option B ==> is a new innovative treatment

  prob_optionB_succ = 0.80,
  prob_optionB_fail = 0.20,

  # if option B fails ==> baseline treatment for relapse
  prob_treatment_succ = 0.55,
  prob_treatment_fail = 0.45
)

costs <- list(
  c_surgery_succ = 9000,
  c_surgery_fail = 11000,
  c_treatment_succ = 2000,
  c_treatment_fail = 4000,
  c_optionB_succ = 9500,
  c_optionB_fail = 11500
)

qalys <- list(
  q_surgery_succ = 8.5,
  q_surgery_fail = 6.5,
  q_treatment_succ = 6,
  q_treatment_fail = 5,
  q_optionB_succ = 9.5,
  q_optionB_fail = 6.5
)


decision_tree <- function(probs, costs, qalys) {
  # whats the total cost and qualys for baseline treatment

  total_cost_treatment <- probs$prob_treatment_succ * costs$c_treatment_succ + probs$prob_treatment_fail * costs$c_treatment_fail
  total_qalys_treatment <- probs$prob_treatment_succ * qalys$q_treatment_succ + probs$prob_treatment_fail * qalys$q_treatment_fail


  # what is the total cost and qalys for the surgery option

  total_cost_surgery <- probs$prob_surgery_succ * costs$c_surgery_succ + probs$prob_surgery_fail * costs$c_surgery_fail
  total_qalys_surgery <- probs$prob_surgery_succ * qalys$q_surgery_succ + probs$prob_surgery_fail * qalys$q_surgery_fail

  # what is the total cost and qalys for the option B

  total_cost_optionB <- probs$prob_optionB_succ * costs$c_optionB_succ + probs$prob_optionB_fail * costs$c_optionB_fail
  total_qalys_optionB <- probs$prob_optionB_succ * qalys$q_optionB_succ + probs$prob_optionB_fail * qalys$q_optionB_fail
   
    #option A

     total_costA <- total_cost_treatment + total_cost_surgery
     total_qalysA <- total_qalys_treatment + total_qalys_surgery

     #option B 

     total_costB <- total_cost_treatment + total_cost_optionB
     total_qalysB <- total_qalys_optionB + total_qalys_treatment

    # calculate teh ICER and the NMB for a defined WTP
    WTP <- 500
    d_costs <- total_costB - total_costA
    d_qalys <- total_qalysB - total_qalysA

    ICER <- d_costs / d_qalys
    NMB <- (WTP * d_qalys) - d_costs
   print(ICER)
    return(ifelse(NMB >0 , print("The option B is cost-effective") , print("The option B is not cost-effective")) )
        

}

decision_tree(probs ,costs ,qalys)


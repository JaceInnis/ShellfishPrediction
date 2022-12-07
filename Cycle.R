





{
  #script to collect gee data and save as .csv of weekly avges
  "What to do about outlying data?"
}


{
  # prepare/load training history object
  {
    #cycle through locations
    {
      #cycle through input data
        # train, val, test split?
        "How to get fair train val test split?"
        "Function to make input data"
      {
        #cycle through hyper parameters
        {
          #train for one epoch
          #apply validation metric
          #save model if it outperforms the best model in loc,datainput,hyperparameter cycle
          #write training history line if best model (epoc #)
          "what if same validation results?"
        }
      }
    }
  }
}



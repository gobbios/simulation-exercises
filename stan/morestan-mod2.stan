data {
  int<lower=0> Ngroups;
  int<lower=0> Ncases;
  //vector[Ngroups] grouplevelresponse;
  //int Ncasespergroup[Ngroups];
  vector[Ncases] subjectlevelpred;
  vector[Ncases] subjectlevellowpred;
  int<lower=1, upper=Ngroups> subjectlevelgroup[Ncases] ;
  //matrix[Ncases, Ngroups] locmat;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> sigma;
  // vector<lower=0>[Ngroups] g_sigma;
  // vector[Ngroups] g_meanval;
  //vector[Ngroups] g_slopes;
  vector[Ngroups] group_intercepts;
  real<lower=0> sigma_group;     //group sd
}
model {
  //real mu;
  //prior
  group_intercepts ~ normal(0, sigma_group);    //group random effects

  for (n in 1:Ncases) {
    //mu = intercept + group_intercepts[subjectlevelgroup[n]] + slope*subjectlevellowpred[n];
    //subjectlevelpred[n] ~ normal(mu, sigma);
    subjectlevelpred[n] ~ normal(intercept + group_intercepts[subjectlevelgroup[n]] + slope*subjectlevellowpred[n], sigma);


    // get subject-level data for the predictor
    // vector[Ncasespergroup[n]] tempdata;
    // int pos;
    // pos = 1;
    // for (i in 1:rows(locmat)) {
    //   if (locmat[i, n]) {
    //     tempdata[pos] = subjectlevelpred[i];
    //     pos = pos + 1;
    //   }
    // }
    // model group level predictor
    //
    //g_sigma[n] ~ normal(1, 3);
    //g_meanval[n] ~ normal(tempdata, g_sigma[n]);
    // the actual model on group level
    //grouplevelresponse[n] ~ normal(intercept + slope * g_meanval[n], sigma);
  }
}

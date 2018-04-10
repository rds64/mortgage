#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
float WAC(std::vector<float> balance, std::vector<float> rate) {
    float total_balance = 0;
    float wac = 0;
    std::vector<float> weights(balance.size());
        
    for (int i = 0; (unsigned)i <= balance.size(); ++i) {
        total_balance += balance[i]; 
    }
    
    for(int i = 0; (unsigned)i <= balance.size(); ++i) {
        weights[i] = rate[i] * (balance[i] / total_balance);
    }
       
    for (int i = 0; (unsigned)i <= balance.size(); ++i) {
        wac += weights[i];
    }
    
    return wac;
}

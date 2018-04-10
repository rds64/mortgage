#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List amortize(double principal, int term, float rate) {
    std::vector<float> age(term + 1);
    std::vector<float> bal(term + 1);
    std::vector<float> pmt(term + 1);
    std::vector<float> inst(term + 1);
    std::vector<float> prin(term + 1);

    for(int i = 0; i <= (term + 1); ++i) {
        bal[i] = principal * (std::pow((1 + rate),term) - std::pow((1 + rate),i)) / (std::pow((1 + rate),term) - 1 );
        pmt[i] = (principal * rate) / (1 - std::pow(1 + rate, -1 * term));
        inst[i] = bal[i] * rate;
        prin[i] = pmt[i] - inst[i];
        age[i] = i;
    }
    List amort = Rcpp::List::create(
            Rcpp::Named("Age") = age,
            Rcpp::Named("Balance") = bal,
            Rcpp::Named("Payment") = pmt,
            Rcpp::Named("Interest") = inst,
            Rcpp::Named("Principal") = prin
    );
    return amort;
}


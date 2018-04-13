#include <Rcpp.h>
#include <sstream> 
using namespace Rcpp;
using namespace std;

/*
    Formrula:
    iA / [1 − (1+i)^-N]
*/

//[[Rcpp::export]]
long double pmt(double principal, int term, double rate) {
    
    return (principal * rate) / (1 - std::pow(1 + rate, -1 * term));
}

/*
    Function Checks to see if provided payment and calculated payment from provided
    loan terms are the same.
 */

//[[Rcpp::export]]
bool check(double balance, long double payment, int term, double rate) {
    long double checked_pmt;
    bool checked;
    checked_pmt = pmt(balance, term, rate);
/*    cout << payment;
    cout << "\n";
    cout << checked_pmt;
*/    
    checked = std::fabs(checked_pmt - payment) < 1E-3;
    return checked;
}

/*
    Formula:
 
        f(i) = P − P (1+i)^-N − iA
  i - ------------------------------
        f′(i) = N P (1+i)^(-N-1) − A
 
*/

//[[Rcpp::export]]
long double solve_rate(float balance, float payment, int term, float start = .05) {
    long double initial;
    bool converge;
    int iterations;
    
    initial = start - ((payment - payment * std::pow(1 + start, -1 * term) - (start * balance)) / 
        (term * payment * std::pow(1 + start, -1 * term - 1) - balance));
    
    converge = check(balance, payment, term, initial);
    iterations = 0;
    
    while (!converge) {
      
        initial = initial - ((payment - payment * std::pow(1 + initial, -1 * term) - (initial * balance)) / (term * payment * std::pow(1 + initial, -1 * term - 1) - balance));
        converge = check(balance, payment, term, initial);
//        cout << initial << "/n";
               
        if (iterations == 100) break;  
        iterations++; 
//        cout << iterations << "\n";
    }
    
    if (!converge) {
        throw "Algorithm Failed to Converge!";
    } 
    
    std::string str ("Algorithm Converged After # Iterations");
//  str.replace(27, 1, char(iterations));
    
//    cout << str;
    return initial;
}


/*** R
    test_pmt <- .Call("_mortgage_pmt", 100000, 360, .04/ 12)
    test_pmt
    .Call("_mortgage_check", 100000, test_pmt, 360, .04 / 12)
    .Call("_mortgage_solve_rate", 100000, 477.4169, 360, .075 / 12) * 12
*/

    
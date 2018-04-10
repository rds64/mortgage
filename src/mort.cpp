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
    long double m_rate;
    m_rate = rate / 12;
    
    return (principal * m_rate) / (1 - std::pow(1 + m_rate, -1 * term));
}

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
  -------------------------------
    f′(i) = N P (1+i)^(-N-1) − A
 
*/

//[[Rcpp::export]]
long double solve_rate(float balance, float payment, int term, float start = .05) {
    long double m_start;
    long double initial;
    bool converge;
    int iterations;
    
    m_start = start / 12;
    
    initial = m_start - ((payment - payment * std::pow(1 + m_start, -1 * term) - (m_start * balance)) / 
        (term * payment * std::pow(1 + m_start, -1 * term - 1) - balance));
    
    converge = check(balance, payment, term, initial);
    iterations = 0;
    
    while (!converge) {
      
        initial = initial - ((payment - payment * std::pow(1 + initial, -1 * term) - (initial * balance)) / (term * payment * std::pow(1 + initial, -1 * term - 1) - balance));
        converge = check(balance, payment, term, initial * 12);
        cout << initial << "/n";
               
        if (iterations == 100) break;  
        iterations++; 
//        cout << iterations << "\n";
    }
    
    if (!converge) {
        throw "Algorithm Failed to Converge!";
    } 
    
    std::string str ("Algorithm Converged After # Iterations");
//  str.replace(27, 1, char(iterations));
    
    cout << str;
    return (initial * 12);
}


/*** R
    test_pmt <- .Call("_mortgage_pmt", 100000, 360, .04)
    test_pmt
    .Call("_mortgage_check", 100000, test_pmt, 360, .04)
    .Call("_mortgage_solve_rate", 100000, 477.4169, 360, .075)
*/

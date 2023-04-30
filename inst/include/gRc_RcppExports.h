// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_gRc_RCPPEXPORTS_H_GEN_
#define RCPP_gRc_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace gRc {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("gRc", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("gRc", "_gRc_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in gRc");
            }
        }
    }

    inline double trAW_(NumericMatrix rA, NumericMatrix rW) {
        typedef SEXP(*Ptr_trAW_)(SEXP,SEXP);
        static Ptr_trAW_ p_trAW_ = NULL;
        if (p_trAW_ == NULL) {
            validateSignature("double(*trAW_)(NumericMatrix,NumericMatrix)");
            p_trAW_ = (Ptr_trAW_)R_GetCCallable("gRc", "_gRc_trAW_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAW_(Shield<SEXP>(Rcpp::wrap(rA)), Shield<SEXP>(Rcpp::wrap(rW)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double trAWB_(NumericMatrix rA, NumericMatrix rW, NumericMatrix rB) {
        typedef SEXP(*Ptr_trAWB_)(SEXP,SEXP,SEXP);
        static Ptr_trAWB_ p_trAWB_ = NULL;
        if (p_trAWB_ == NULL) {
            validateSignature("double(*trAWB_)(NumericMatrix,NumericMatrix,NumericMatrix)");
            p_trAWB_ = (Ptr_trAWB_)R_GetCCallable("gRc", "_gRc_trAWB_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAWB_(Shield<SEXP>(Rcpp::wrap(rA)), Shield<SEXP>(Rcpp::wrap(rW)), Shield<SEXP>(Rcpp::wrap(rB)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double trAWBW_(NumericMatrix rA, NumericMatrix rW, NumericMatrix rB) {
        typedef SEXP(*Ptr_trAWBW_)(SEXP,SEXP,SEXP);
        static Ptr_trAWBW_ p_trAWBW_ = NULL;
        if (p_trAWBW_ == NULL) {
            validateSignature("double(*trAWBW_)(NumericMatrix,NumericMatrix,NumericMatrix)");
            p_trAWBW_ = (Ptr_trAWBW_)R_GetCCallable("gRc", "_gRc_trAWBW_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAWBW_(Shield<SEXP>(Rcpp::wrap(rA)), Shield<SEXP>(Rcpp::wrap(rW)), Shield<SEXP>(Rcpp::wrap(rB)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double trAWBV_(NumericMatrix rA, NumericMatrix rW, NumericMatrix rB, NumericMatrix rV) {
        typedef SEXP(*Ptr_trAWBV_)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_trAWBV_ p_trAWBV_ = NULL;
        if (p_trAWBV_ == NULL) {
            validateSignature("double(*trAWBV_)(NumericMatrix,NumericMatrix,NumericMatrix,NumericMatrix)");
            p_trAWBV_ = (Ptr_trAWBV_)R_GetCCallable("gRc", "_gRc_trAWBV_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAWBV_(Shield<SEXP>(Rcpp::wrap(rA)), Shield<SEXP>(Rcpp::wrap(rW)), Shield<SEXP>(Rcpp::wrap(rB)), Shield<SEXP>(Rcpp::wrap(rV)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline NumericVector trAWBlist_(List Alist, NumericMatrix W, List Blist, int mode = 0) {
        typedef SEXP(*Ptr_trAWBlist_)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_trAWBlist_ p_trAWBlist_ = NULL;
        if (p_trAWBlist_ == NULL) {
            validateSignature("NumericVector(*trAWBlist_)(List,NumericMatrix,List,int)");
            p_trAWBlist_ = (Ptr_trAWBlist_)R_GetCCallable("gRc", "_gRc_trAWBlist_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAWBlist_(Shield<SEXP>(Rcpp::wrap(Alist)), Shield<SEXP>(Rcpp::wrap(W)), Shield<SEXP>(Rcpp::wrap(Blist)), Shield<SEXP>(Rcpp::wrap(mode)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector trAWBWlist_(List Alist, NumericMatrix W, List Blist, int mode = 0) {
        typedef SEXP(*Ptr_trAWBWlist_)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_trAWBWlist_ p_trAWBWlist_ = NULL;
        if (p_trAWBWlist_ == NULL) {
            validateSignature("NumericVector(*trAWBWlist_)(List,NumericMatrix,List,int)");
            p_trAWBWlist_ = (Ptr_trAWBWlist_)R_GetCCallable("gRc", "_gRc_trAWBWlist_");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_trAWBWlist_(Shield<SEXP>(Rcpp::wrap(Alist)), Shield<SEXP>(Rcpp::wrap(W)), Shield<SEXP>(Rcpp::wrap(Blist)), Shield<SEXP>(Rcpp::wrap(mode)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

}

#endif // RCPP_gRc_RCPPEXPORTS_H_GEN_

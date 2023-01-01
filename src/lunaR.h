
//    --------------------------------------------------------------------
//
//    This file is part of Luna.
//
//    LUNA is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    Luna is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with Luna. If not, see <http://www.gnu.org/licenses/>.
//
//    Please see LICENSE.txt for more details.
//
//    --------------------------------------------------------------------

#ifndef __LUNAR_H__
#define __LUNAR_H__

#define R_NO_REMAP

#include <string>
#include "luna.h"

extern "C" { 

#include <R.h>
#include <Rdefines.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

  // initialize luna library
  
  void R_init_luna(DllInfo *info);

  // flush log
  
  void Rflush_log();

  // attach an EDF (ID, annots)
  
  SEXP Rattach_edf( SEXP x , SEXP y , SEXP z );

  // add an annotation file (i.e. after attaching the EDF)
  
  void Radd_annot( SEXP a );

  // add an annotation from R to an existing EDF
  
  void Radd_annot_fromR( SEXP n , SEXP a );

  // basic stats on the loaded file

  SEXP Rdesc();
  
  // simply console report of current in-memory EDF

  SEXP Rstat();

  // is problem flag set?
  
  SEXP Rproblem();
  void Rsetproblem( SEXP x );

  // set Luna variables
  
  void Rset_var( SEXP x , SEXP y);

  // get Luna variables

  SEXP Rshow_var( SEXP x );

  // return vector of channel names

  SEXP Rchannels();

  // return vector of annotations

  SEXP Rannots();

  // return all intervals for an annotation
  
  SEXP Rannot( SEXP ann );


  // return dataframe describing epochs and mask (and annotations)

  SEXP Rmask( SEXP ann );
  
  // set log-mode (0/1)

  SEXP Rlogmode( SEXP i );

  // evaluate a command

  SEXP Reval_cmd( SEXP x );


  // set up to evaluate commands over a sample-list

  void Reval_cmd_noreturns( SEXP x );

  void Reval_init_returns();

  SEXP Reval_get_returns();


  // get an individual from a destrat databasem import as a retval structure

  SEXP Rdb2retval( SEXP x , SEXP y );

  // dsiplay R output in a list format

  SEXP Rout_list( retval_t & );
 
  // extract a matrix (dataframe) of raw data

  SEXP Rmatrix_epochs( SEXP e , SEXP ch , SEXP ann  );

  SEXP Rmatrix_intervals( SEXP i , SEXP ch , SEXP ann );

  SEXP Rmatrix_internal( const std::vector<interval_t> &, 
			 const std::vector<int> * ,
			 const signal_list_t & , 
			 const std::map<std::string,int> & );



  // apply a function over epochs

  SEXP Riterate( SEXP fn , SEXP chs , SEXP annots, SEXP byannot , SEXP w, SEXP rho );


  // clear/reset EDF store  ( ldrop() )

  void Rdrop();


  // set error handler

  void R_moonlight_mode();
  

  // clear writer (should not be called by user)
  
  void Rclear_out();
  

  // clear/reset cmd_t::vars  ( lreset() )

  void Rclear_vars();

  //
  // Misc wrappers
  //
  
  SEXP R1d_denoise( SEXP x , SEXP l );

  SEXP R_filter( SEXP x , SEXP Rsr, SEXP Rlwr , SEXP Rupr , SEXP Rtw , SEXP Rripple );

  //
  // Helper functions to report errors, etc
  //

  void R_error( const std::string & );

  void R_warning( const std::string & );

  // helper functions
  SEXP Rmake_strvector( const std::vector<std::string> & r );
  SEXP Rmake_dblvector( const std::vector<double> & r );
  SEXP Rmake_intvector( const std::vector<int> & r );
  
}


//
// internal helper functions
//

std::vector<std::string> Rluna_to_strvector( SEXP );
std::vector<int>    Rluna_to_intvector( SEXP );
std::vector<double> Rluna_to_dblvector( SEXP );



struct Rdata_t {
    
  edf_t edf;

  std::string id;


  // increase we need to expand beyond the interval of the EDF in annotation space  
  // this can be >= than the EDF total_size

  uint64_t total_size;
  
  // annotations

  std::map<std::string,annot_t*> annots;
  
  bool add_annotations( const std::string & filename );
  
};


struct Rfndata {
  //  Rdisplay_options * opt;
  SEXP fncall;
  SEXP rho;
};



void R_bail_function( const std::string & msg )
{
  R_error( msg );
  return;
}

void R_moonlight_bail_function( const std::string & msg )
{
  Rprintf( ("error: " + msg).c_str() );
  return;
}

void R_message_function( const std::string & msg )
{
  Rprintf( msg.c_str() );
  return;
}


//
// misc/tests 
//

std::string Rversion();


//
// Assist struct
//

struct retval_indiv_strata_t { 
retval_indiv_strata_t( const retval_indiv_t & indiv , const retval_strata_t & strata ) 
: indiv(indiv) , strata(strata) { } 
  retval_indiv_t indiv;
  retval_strata_t strata;
  bool operator<( const retval_indiv_strata_t & rhs ) const 
  {
    // needs strata-first ordering to match the below
    // can change this potentially
    if ( strata < rhs.strata ) return true;
    if ( rhs.strata < strata ) return false;
    return indiv < rhs.indiv;
  }
};


#endif

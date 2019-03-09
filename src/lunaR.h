
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

  // initialize luna library
  
  void R_init_luna(DllInfo *info);

  // flush log
  
  void Rflush_log();

  // attach an EDF (ID, annots)
  
  SEXP Rattach_edf( SEXP x , SEXP y , SEXP z );

  // basic stats on the loaded file

  SEXP Rdesc();
  
  // simply console report of current in-memory EDF

  SEXP Rstat();

  // set Luna variables
  
  void Rset_var( SEXP x , SEXP y);

  // clear all Luna variables (and return globals to default values)
  
  void Rclear_vars();

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

  // clear/reset EDF store

  void Rclear();


  //
  // Helper functions to report errors, etc
  //

  void R_error( const std::string & );

  void R_warning( const std::string & );

  SEXP Rmake_string_vector( const std::vector<std::string> & r );
  
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


//
// misc/tests 
//

std::string Rversion();



#endif

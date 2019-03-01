
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
  
  void R_init_rluna(DllInfo *info);

  // attach an EDF
  
  SEXP Rattach_edf( SEXP x , SEXP y );

  // basic stats on the loaded file

  SEXP Rdesc();
  
  // epoch data 
  
  SEXP Repoch_data( SEXP e , SEXP i );

  // evaluate a command

  SEXP Reval_cmd( SEXP x );

  // get an individual from a destrat databasem import as a retval structure

  SEXP Rdb2retval( SEXP x , SEXP y );

  // dsiplay R output in a list format

  SEXP Rout_list( retval_t & );
 
  // extract raw signal data (by epoch)

  SEXP Rextract_my_signals_by_epoch( SEXP e , SEXP chs );
  
  // extract raw signal data, internal function

  SEXP Rextract_my_signals_internal( const interval_t & , const signal_list_t & );

  // apply a function over epochs

  SEXP Riterate( SEXP fn , SEXP e , SEXP rho );

  //
  // Helper functions to report errors, etc
  //

  void R_error( const std::string & );
  void R_warning( const std::string & );

    // clear/reset EDF store
  void Rclear();

  
}


//
// internal helper functions
//

std::vector<std::string> Rluna_to_strvector( SEXP );
std::vector<int>    Rluna_to_intvector( SEXP );
std::vector<double> Rlunato_dblvector( SEXP );


struct Rdata_t {
  
  edf_t edf;
  std::string id;

  // increase we need to expand beyond the interval of the EDF in annotation space  
  // this can be >= than the EDF total_size
  void update_size();
  uint64_t total_size;
  
  // annotations
  std::map<std::string,annot_t*> annots;
  bool add_annotations( const std::string & filename );

  //   std::map<std::string,std::vector<std::string> > annot_files;

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

  // misc/tests 
  std::string Rversion();

  SEXP Rtest( SEXP x );


#endif

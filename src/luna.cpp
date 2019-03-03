
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


#include "luna.h"

extern globals global;

#include <iostream>
#include <string>

// a single, global instance

Rdata_t * rdata;

std::string Rversion()
{
  std::string vmaj = R_MAJOR;
  std::string vmin = R_MINOR;  
  return vmaj + "-" + vmin;
}


void R_init_luna(DllInfo *info)
{

  global.init_defs();

  // redefine error handler
  globals::bail_function = &R_bail_function;

  // turn off external output
  writer.nodb();
  
  // use this as a general store  
  rdata = NULL;
  
  // indicate that we are running inside R
  global.R();

  std::string msg = "** luna " ;
  msg += globals::version ;
  msg += " ";
  msg += globals::date;
  msg += "\n";
  
  Rprintf( msg.c_str() );
}



SEXP Rlogmode( SEXP i )
{
  // will only be called with a single integer
  //  int m = INTEGER(i)[0];
 
  // set 'm' mode

  // all done
  return( R_NilValue );
}



SEXP Rstat()
{

  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }


  // Record duration, as hh:mm:ss string
  uint64_t duration_tp = globals::tp_1sec
    * (uint64_t)rdata->edf.header.nr
    * rdata->edf.header.record_duration;
  
  std::string total_duration_hms = Helper::timestring( duration_tp );

  std::stringstream ss;
  
  ss << rdata->edf.id << " : "
     << rdata->edf.header.ns << " signals, "
     << Helper::timestring( duration_tp ) ;
  
  if ( rdata->edf.timeline.epoched() )
    {
      ss << ", " << rdata->edf.timeline.num_epochs() 
	 << " unmasked epochs, " 
	 << rdata->edf.timeline.num_total_epochs() - rdata->edf.timeline.num_epochs()  << " masked";
    }
  
  ss << "\n";

  Rprintf( ss.str().c_str() );
  
  return( R_NilValue );

}

SEXP Rdesc()
{
  
  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }


  // EDF header and other information returned as an R list
  
  //1 EDF
  //2 ID
  //3 DUR
  //4 EPOCH_N
  //5 EPOCH_SEC

  //6 CH
  //   NAME
  //   FS

  //7 ANNOT
  //

  int protect = 0 ;
  
  SEXP r;
  PROTECT( r = Rf_allocVector( VECSXP, 11 )); 
  ++protect;
  
  SEXP r_names;
  PROTECT( r_names = Rf_allocVector( STRSXP, 11 )); 
  ++protect;
  
  SET_STRING_ELT( r_names , 0, Rf_mkChar( "EDF" ) );
  SET_STRING_ELT( r_names , 1, Rf_mkChar( "ID" ) );
  SET_STRING_ELT( r_names , 2, Rf_mkChar( "DUR" ) );
  SET_STRING_ELT( r_names , 3, Rf_mkChar( "SEC" ) );

  SET_STRING_ELT( r_names , 4, Rf_mkChar( "NS" ) );
  SET_STRING_ELT( r_names , 5, Rf_mkChar( "NS_ALL" ) );
  
  SET_STRING_ELT( r_names , 6, Rf_mkChar( "EPOCH_N" ) );
  SET_STRING_ELT( r_names , 7, Rf_mkChar( "EPOCH_N1" ) );
  SET_STRING_ELT( r_names , 8, Rf_mkChar( "EPOCH_SEC" ) );

  SET_STRING_ELT( r_names , 9, Rf_mkChar( "CHS" ) );
  SET_STRING_ELT( r_names , 10, Rf_mkChar( "ANNOTS" ) );	     
  Rf_setAttrib( r, R_NamesSymbol, r_names); 

  //
  // set data  
  //
  
  // EDF filename
  SET_VECTOR_ELT( r , 0, PROTECT( Rf_mkString( rdata->edf.filename.c_str() ) ) );
  ++protect;
  
  // ID
  SET_VECTOR_ELT( r , 1, PROTECT( Rf_mkString( rdata->edf.id.c_str() ) ) );
  ++protect;
  
  // Record duration, as hh:mm:ss string
  uint64_t duration_tp = globals::tp_1sec
    * (uint64_t)rdata->edf.header.nr
    * rdata->edf.header.record_duration;
  
  std::string total_duration_hms = Helper::timestring( duration_tp );

  SET_VECTOR_ELT( r , 2 , PROTECT( Rf_mkString( total_duration_hms.c_str() ) ) );
  ++protect;

  // Record duration, in seconds (double)
  SET_VECTOR_ELT( r , 3 , PROTECT( Rf_ScalarReal( (double) rdata->edf.header.nr
						  * rdata->edf.header.record_duration ) ) ) ;
  ++protect;
  
  // number of signals (in mem)
  SET_VECTOR_ELT( r , 4 , PROTECT( Rf_ScalarInteger( rdata->edf.header.ns ) ) ) ;
  ++protect;

  // number of signals (in file, i.e. total)
  SET_VECTOR_ELT( r , 5 , PROTECT( Rf_ScalarInteger( rdata->edf.header.ns_all ) ) ) ;
  ++protect;
  
  if ( rdata->edf.timeline.epoched() )
    {

      SET_VECTOR_ELT( r , 6 , PROTECT( Rf_ScalarInteger( rdata->edf.timeline.num_total_epochs() ) ) ) ;
      ++protect;
      
      SET_VECTOR_ELT( r , 7 , PROTECT( Rf_ScalarInteger( rdata->edf.timeline.num_epochs() ) ) ) ;
      ++protect;

      SET_VECTOR_ELT( r , 8 , PROTECT( Rf_ScalarInteger( rdata->edf.timeline.epoch_length() ) ) ) ;
      ++protect;

    }
  else
    {
      SET_VECTOR_ELT( r , 6 , PROTECT( Rf_ScalarInteger( NA_INTEGER ) ) ) ;
      ++protect;
      
      SET_VECTOR_ELT( r , 7 , PROTECT( Rf_ScalarInteger( NA_INTEGER ) ) ) ;
      ++protect;

      SET_VECTOR_ELT( r , 8 , PROTECT( Rf_ScalarInteger( NA_INTEGER ) ) ) ;
      ++protect;

    }


  //
  // Channels
  //
  
  const int ns = rdata->edf.header.ns;
  SEXP chs;
  PROTECT( chs = Rf_allocVector( VECSXP, ns )); 
  ++protect;

  SEXP chs_names;
  PROTECT( chs_names = Rf_allocVector( STRSXP, ns )); 
  ++protect;
  
  for (int s=0;s<ns;s++)
    {
      int fs = rdata->edf.header.n_samples[s];
      SET_VECTOR_ELT( chs , s , PROTECT( Rf_ScalarInteger( fs ) ) ) ;
      ++protect;
      
      SET_STRING_ELT( chs_names , s ,
		      PROTECT( Rf_mkChar( Helper::sanitize( rdata->edf.header.label[s] ) .c_str() ) ) ) ;
      ++protect;
      
    }
  
  Rf_setAttrib( chs, R_NamesSymbol, chs_names); 

  SET_VECTOR_ELT( r , 9 , chs );

  // Annotations
  
  const int na = 1;

  SEXP annots;
  PROTECT( annots = Rf_allocVector( VECSXP, na )); 
  ++protect;
  SEXP annots_names;
  PROTECT( annots_names = Rf_allocVector( STRSXP, na )); 
  ++protect;
  Rf_setAttrib( annots, R_NamesSymbol, annots_names); 

  SET_VECTOR_ELT( r , 10 , annots );

  // All done
  UNPROTECT( protect );
  return r;
      
}


SEXP Rtest( SEXP x )
{
  SEXP result;
  PROTECT(result = NEW_INTEGER(1));
  INTEGER(result)[0] = INTEGER(x)[0] * 2;
  UNPROTECT(1);
  return result;  
}


void R_error( const std::string & s )
{
  Rf_error( s.c_str() ) ; 
} 


void R_warning( const std::string & s )
{
  Rf_warning( s.c_str() ) ; 
} 


//
// attach an EDF
//

SEXP Rattach_edf( SEXP x , SEXP id , SEXP ann )
{

  std::string edf_file = CHAR( STRING_ELT( x , 0 ) );

  std::string edf_id   = CHAR( STRING_ELT( id , 0 ) );

  std::vector<std::string> annots = Rluna_to_strvector( ann );

  // check EDF exists
  if ( ! Helper::fileExists( edf_file ) )
    Helper::halt( "cannot find " + edf_file );

  // clear any old data
  if ( rdata != NULL ) delete rdata;
  
  rdata = new Rdata_t;
  
  bool okay = rdata->edf.attach( edf_file , edf_id );

  if ( ! okay ) 
    Helper::halt( "problem attaching EDF" + edf_file );

  // attach annotations
  for (int a=0;a<annots.size();a++)
    rdata->add_annotations( annots[a] );
  
  return(R_NilValue);
}


void Rclear()
{
  if ( rdata != NULL )
    {
      delete rdata;
      rdata = NULL;
    }
}


//
// Attach annotations
//

bool Rdata_t::add_annotations( const std::string & annotfile )
{
  
  // is 'annotfile' in fact a folder (i.e. ending in '/') ? 
  
  if ( annotfile[ annotfile.size() - 1 ] == '/' )
    {

      // this means we are specifying a folder, in which case search for all files that                                                                             
      // start id_<ID>_* and attach thoses                                                                                                                          
      DIR * dir;
      struct dirent *ent;
      if ( (dir = opendir ( annotfile.c_str() ) ) != NULL )
	{
	  /* print all the files and directories within directory */
	  while ((ent = readdir (dir)) != NULL)
	    {
	      std::string fname = ent->d_name;
	      
	      if ( Helper::file_extension( fname , "ftr" ) ||
		   Helper::file_extension( fname , "xml" ) ||
		   Helper::file_extension( fname , "eannot" ) ||
		   Helper::file_extension( fname , "annot" ) )
		{
		  edf.load_annotations( annotfile + fname );
		}
	    }
	  closedir (dir);
	}
      else
	Helper::halt( "could not open folder " + annotfile );
    }

  //
  // else a single file, load it
  //

  else
   {
     edf.load_annotations( annotfile );
   }


  return true;

  // old code, we can remove...
  
//   const std::string & annot_name = edf.flist[ annotfile ];
  
//   // Connect to the timeline
//   annot_t * myannot = edf.timeline.annotations.add( annot_name );
      
//   loaded = myannot->load( annotfile );
  
//   return loaded; 
        
}

void Rdata_t::update_size()
{
  // for all annotations
  
  total_size = edf.timeline.last_time_point_tp + 1;

  std::map<std::string,annot_t*>::const_iterator ai = edf.timeline.annotations.annots.begin();
  while ( ai != edf.timeline.annotations.annots.end() )
    {

      annot_t * annot = ai->second;

      uint64_t max_tp = annot->maximum_tp();
      
      //      std::cout << "max t = " << ai->first << " --> " << max_tp << "\n";
      
      if ( max_tp >= total_size )
        {
          total_size = max_tp + 1;
          //std::cout << "adjusting total size to " << total_size << "\n";
        }
      ++ai;
    }

}


SEXP Rfetch_annots()
{

  // get names of all annotations
  std::vector<std::string> annot_labels = rdata->edf.timeline.annotations.names();
  
  // find an annotation based on its name
  std::string label = "a1";
  annot_t * a = rdata->edf.timeline.annotations.find( label );
  if ( a != NULL ) { // ...
  }
  
  
//   // get all specific annots, given annot_t * a 

//   // these two accumulate all annotations
//   std::map<std::string,interval_evt_map_t> allannots;
//   std::set<feature_t> features; // interval + labels

  
//   std::string aclass = a->name;

//   // total_size is updated as annotations are loaded in, i.e.
//   // in case some are larger than the EDF signal
  
//   allannots[ aclass ] = a->extract( interval_t( 0 , rdata->total_size ) );

//   // for these annotation, get the specific list of events
  
//   interval_evt_map_t & tmp = allannots[ a->name ];

//   interval_evt_map_t::const_iterator ii = tmp.begin();
//   while ( ii != tmp.end() )
//     {
	  
//       feature_t feature;

//       feature.feature = ii->first;
//       feature.window = ii->first;

//       // get event(s)
      
//       const std::vector<const event_t*> & events = ii->second;
//       std::string label = "";
//       for (int e=0;e<events.size();e++)
// 	{
// 	  if ( e!=0 ) label += ",";
// 	  label += events[e]->label;
// 	  feature.add_data( events[e]->metadata );
// 	}
      
      
//       if ( label != aclass )
// 	feature.signal = label;
      
//       feature.label = aclass;
      
//       // display

//       std::cout << "\n" << feature.as_string( "\t" ) << "\n";
//       std::cout << feature.print_data( )  << "\n";
    
//       // store                                                                                                                         
//       features.insert( feature );
      
//       // next interval                                                                                                                 
//       ++ii;
//     }
 
  return( R_NilValue );

}






//
// evaluate a command
//

SEXP Reval_cmd( SEXP x )
{
  
  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }
  
  std::string cmdstr = CHAR( STRING_ELT( x , 0 ) );

  retval_t ret;

  writer.use_retval( &ret );

  Rprintf( "evaluating...\n" );
  
  // set command string

  cmd_t cmd( cmdstr );
  
  // eval on the current EDF

  cmd.eval( rdata->edf );
  
  // swtich 'off' this stream for the next command
  // as it will get deleted on leaving this function
  
  writer.use_retval( NULL );
  
  // and we need to completely clear the writer (i.e. so that old
  // factor/level labels are not in place for the next run, etc)
  
  writer.clear();
  
  // convert retval_t to R list and return 

  return Rout_list( ret );
 
}


//
// pull a retval structure from an existing output database
//

SEXP Rdb2retval( SEXP x , SEXP y )
{

  std::string dbstr = CHAR( STRING_ELT( x , 0 ) );

  std::string idstr = CHAR( STRING_ELT( y , 0 ) );

  retval_t ret = writer_t::dump_to_retval( dbstr , idstr );

  // convert retval_t to R list and return 

  return Rout_list( ret );

}


//
// structure output in list format for R
//

SEXP Rout_list( retval_t & r )
{

  // list [ cmd ]
  //    list [ table ]  e.g..  F_CH
  //            data.frame   :  cols = facs + vars  ; rows = lvls + values 

  // e.g.    F    CH   DENS  AMP
  //         11   C3   1.23  0.23
  //         16   C4   1.23  0.23

  // keep track of items on stack 
  int protect = 0;
	  
  // # of commands
  const int nc = r.data.size();
  
  SEXP c_list;
  PROTECT( c_list = Rf_allocVector( VECSXP, nc )); 
  ++protect;
  
  // command labels
  SEXP c_names;
  PROTECT( c_names = Rf_allocVector( STRSXP, nc )); 
  ++protect;
  
  int cmd_cnt = 0;

  // iterate over each command
  
  retval_data_t::iterator cc = r.data.begin();
  while ( cc != r.data.end() )
    {      
      
      const retval_cmd_t & cmd = cc->first;

      // number of virtual tables/factors for this command 
      
      const int nt = cc->second.size();
      
      SEXP t_list;
      PROTECT( t_list = Rf_allocVector( VECSXP, nt )); 
      ++protect;
      
      // command labels
      SEXP t_names;
      PROTECT( t_names = Rf_allocVector( STRSXP, nt )); 
      ++protect;
      
      int t_cnt = 0;
      
      // factors/tables
      
      std::map<retval_factor_t,
	       std::map<retval_var_t,
			std::map<retval_strata_t,
				 retval_value_t > > >::iterator tt = cc->second.begin();

      //std::cout << " (found " << nt << " tables)\n" ;

      
      while ( tt != cc->second.end() )
	{
	  
	  
	  const retval_factor_t & table = tt->first; 
	  
	  //std::cout << " considering table ...\n";
	  
	  //
	  // for this particular cmd/fac combination, make a data-frame
	  //
	  
	  //    cols = factors + variables
	  //    rows = levels
	  //    items = values

	  // in this table ( tt ):
	  // how many factors (i.e. F CH == 2 )
	  // how many variables

	  // but we need to split these by type
	  
	  const int nf = table.factors.size();
	  
	  const int nv = tt->second.size();
	  
	  const int ncols = nf + nv;

	  //std::cout << " has " << ncols << " cols, " << nf << " fac, " << nv << " vars\n";

	  //
	  // quickly scan for the a) number of rows
	  // and b) whether the factors are string, double or int
	  //

	  // str > dbl > int
	  std::set<std::string> int_factor, str_factor, dbl_factor;
	  
	  std::set<retval_strata_t> rows;
		   
	  std::map<retval_var_t,
		   std::map<retval_strata_t,
			    retval_value_t > >::iterator vv = tt->second.begin();

	  while ( vv != tt->second.end() )
	    {

	      std::map<retval_strata_t, retval_value_t >::iterator ss = vv->second.begin();
	      while ( ss != vv->second.end() )
		{

		  const retval_strata_t & s = ss->first;
		  
		  rows.insert( s );

		  std::set<retval_factor_level_t>::iterator ll = s.factors.begin();
		  while ( ll != s.factors.end() )
		    {
		      if      ( ll->is_str ) str_factor.insert( ll->factor );
		      else if ( ll->is_dbl ) dbl_factor.insert( ll->factor );
		      else if ( ll->is_int ) int_factor.insert( ll->factor );
		      ++ll;
		    }
		  
		  ++ss;
		}
	      ++vv; 
	    }
	  
	  
	  //
	  // Now, we should know the number of rows, and whether a
	  // given factor is string, double or int
	  //
	  
	  const int nrows = rows.size();

	  //std::cout << " and found " << nrows << " rows\n";
	  
	  //
	  // we now need to build a matrix of 'nrows' rows and 'ncols' colums (fac + vars)
	  //
	  
	  SEXP df;     // data.frame	  
	  SEXP cls;    // class attribute
	  SEXP nam;    // col-names
	  SEXP rownam; // row-names
	  
	  // df = a list w/ ncols elements
	  PROTECT( df = Rf_allocVector( VECSXP, ncols ));
	  ++protect;
	  
	  // set class attribute for df
	  PROTECT(cls = Rf_allocVector(STRSXP, 1)); // class attribute
	  ++protect;
	  
	  SET_STRING_ELT(cls, 0, Rf_mkChar( "data.frame" ));
	  Rf_classgets(df, cls);
	  
	  // col-names
	  PROTECT(nam = Rf_allocVector(STRSXP, ncols)); // names attribute (column names)
	  ++protect;
	  

	  int col_cnt = 0;

	  //
	  // Add factors
	  //
	  
	  //std::cout << " wanting to create the data-frame...\n";
	  
	  std::set<std::string>::const_iterator ff = table.factors.begin();
	  while ( ff != table.factors.end() )
	    {

	      bool is_str_factor = false , is_dbl_factor = false , is_int_factor = true;

	      if ( str_factor.find( *ff ) != str_factor.end() )
		is_str_factor = true;
	      else if ( dbl_factor.find( *ff ) != dbl_factor.end() )
		is_dbl_factor = true;
	      
	      //std::cout << " fac -> " << ff->c_str() << "\n";
	      
	      SEXP col;
	      
	      if ( is_str_factor ) 
	       	PROTECT( col = Rf_allocVector( STRSXP , nrows ) );
	      else if ( is_dbl_factor )
	       	PROTECT( col = Rf_allocVector( REALSXP , nrows ) );
	      else
	       	PROTECT( col = Rf_allocVector( INTSXP , nrows ) );

	      ++protect;
	      
	      
	      // consider all factor/level rows
	      int r_cnt = 0;
	      std::set<retval_strata_t>::iterator rr =  rows.begin();
	      while ( rr != rows.end() )
		{
		  
		  retval_factor_level_t lvl = rr->find( *ff );
		  
		  // get value from 'fac', but bear in mind, it may
		  // be of different type (would be v. strange, but
		  // handle here just in case, w/ a cast)
		  
		  if ( is_str_factor )
		    {
		      if ( lvl.is_str ) 
			SET_STRING_ELT( col,r_cnt,Rf_mkChar( lvl.str_level.c_str() ) );
		      else if ( lvl.is_int )
			SET_STRING_ELT( col,r_cnt,Rf_mkChar( Helper::int2str(lvl.int_level).c_str() ) );
		      else if ( lvl.is_dbl )
			SET_STRING_ELT( col,r_cnt,Rf_mkChar( Helper::dbl2str(lvl.dbl_level).c_str() ) );
		    }
		  else if ( is_dbl_factor )
		    {
		      double d = lvl.dbl_level;
		      if ( lvl.is_str ) d = NA_REAL;
		      else if ( lvl.is_int ) d = lvl.int_level;

		      REAL(col)[ r_cnt ] = d;

		    }
		  else if ( is_int_factor )
		    {
		      int i = lvl.int_level;
		      if      ( lvl.is_str ) i = NA_INTEGER;
		      else if ( lvl.is_dbl ) i = (int)lvl.dbl_level;
		      
		      INTEGER(col)[r_cnt ] = i;

		    }
		  		  
		  //std::cout << "found " << lvl.print() << "\n";
		
		  ++r_cnt;
		  ++rr;
		}
	      
	      // add this column to the df
	      SET_VECTOR_ELT( df, col_cnt , col );

	      // and add a name
	      SET_STRING_ELT(nam, col_cnt , Rf_mkChar( ff->c_str() ));
	      
	      // clean up this one column 'col' added
	      
	      UNPROTECT(1);
	      --protect;

	      // next column (factor)
	      ++col_cnt;
	      
	      ++ff;
	    }
	
	  //
	  // Repeat, as for factors, but now adding actual variables 
	  //
		  
	  vv = tt->second.begin();
	  
	  while ( vv != tt->second.end() )
	    {

	      const retval_var_t & var = vv->first;
		
	      // what type of variable is this?
	      // vv->is_string(), vv->is_double(), vv->is_int()
	      
	      ///	      std::cout << " var -> " << var.name << "\n";
	      
	      bool var_is_string = r.var_has_strings.find( var.name ) != r.var_has_strings.end();
	      bool var_is_double = r.var_has_doubles.find( var.name ) != r.var_has_doubles.end();
	      
	      SEXP col;
	      
	      if      ( var_is_string )
	       	PROTECT( col = Rf_allocVector( STRSXP , nrows ) );
	      else if ( var_is_double )
	       	PROTECT( col = Rf_allocVector( REALSXP , nrows ) );
	      else
	       	//PROTECT( col = Rf_allocVector( INTSXP , nrows ) );  // because we might have long long ints returned, make everything REAL ...
   	        PROTECT( col = Rf_allocVector( REALSXP , nrows ) );
	      
	      ++protect;
	      	      
	      // consider all factor/level rows as before (based on same rows file)

	      int r_cnt = 0;

	      std::set<retval_strata_t>::iterator rr =  rows.begin();
	      while ( rr != rows.end() )
		{

		  // i.e. we are ensuring that we are iterating in the same order as we
		  // previously did for each variable, so
		  
		  //
		  // does this variable have a non-missing value for this row/level? 
		  //
		  
		  std::map<retval_strata_t, retval_value_t>::const_iterator yy = vv->second.find( *rr );

		  // not present...
		  if ( yy == vv->second.end() )
		    {
		      // set to NA
		      if      ( var_is_string ) SET_STRING_ELT( col,r_cnt, NA_STRING );
		      else if ( var_is_double ) REAL(col)[ r_cnt ] = NA_REAL;
		  //  else                      INTEGER(col)[ r_cnt ] = NA_INTEGER;
		      else                      REAL(col)[ r_cnt ] = NA_REAL; // as we might have long long ints returned
		      		      
		    }
		  else // ...is present
		    {
		      // because of how sqlite stores numeric values, a double may be cast as an int;
		      // therefore, some values for a double variable may in fact be stored as value.i (i.e. if 1.0, etc)
		      // therefore, we need to check for this special case, for data coming from db2retval at least
		      // (this will all be fine if coming from a luna eval() 
		      
		      if      ( var_is_string ) 
			SET_STRING_ELT( col, r_cnt, Rf_mkChar( yy->second.s.c_str() ) );
		      else if ( var_is_double ) 
			{
			  // special case
			  if ( yy->second.is_int )
			    REAL(col)[ r_cnt ] = yy->second.i ;
			  else
			    REAL(col)[ r_cnt ] = yy->second.d ;
			}
		      else                      
			//INTEGER(col)[ r_cnt ] = yy->second.i ; 
		        REAL(col)[ r_cnt ] = yy->second.i ;  // as we might have long long ints returned...
		      
		    }
		  
		  // next row/lvl
		  ++r_cnt;
		  ++rr;
		}
	      
	      // add this column to the df
	      SET_VECTOR_ELT( df, col_cnt , col );

	      // and add a name
	      SET_STRING_ELT(nam, col_cnt , Rf_mkChar( var.name.c_str() ));
	      
	      // clean up this one column 'col' added
	      
	      UNPROTECT(1);
	      --protect;

	      // next column (variable)
	      ++col_cnt;
	      
	      ++vv;
	    }



	  
	    //   PROTECT(ans1 = Rf_allocVector(INTSXP, 3)); // first column
	    //   PROTECT(ans2 = Rf_allocVector(INTSXP, 3)); // second column
	    //   for (int i=0; i<3; ++i) { // some data
	    // 	INTEGER(ans1)[i] = i+1;
	    // 	INTEGER(ans2)[i] = -(i+1);
	    //   }

	    //   // set data for this column
	    
	    //   //SET_VECTOR_ELT(ret, 1, ans2);
 
	    //   // set column names
	    
	  
	    //   ++col;

	    // }


	  
	  //
	  // attach col-names to df
	  //
	  
	  Rf_namesgets(df, nam);
	  
	  
	  //
	  // row-names
	  //

	  PROTECT(rownam = Rf_allocVector(STRSXP, nrows)); // row.names attribute
	  ++protect;
	  for (int r = 0 ; r < nrows ; r++)
	    {
	      std::string rname = Helper::int2str( r+1 );
	      SET_STRING_ELT(rownam, r, Rf_mkChar( rname.c_str() ) );
	    }
	  Rf_setAttrib( df , R_RowNamesSymbol, rownam);

	  
	  // add this data-frame to the t_list (i.e. all tables for this command)
	  SET_VECTOR_ELT( t_list , t_cnt , df );

	  // label (factors, with _ delim)
	  std::string table_name = Helper::sanitize( Helper::stringize( table.factors , "_" ));
	  if ( table_name == "" ) table_name = "BL";
	  SET_STRING_ELT( t_names , t_cnt , Rf_mkChar( table_name.c_str() ));
	  	  
	  // Next virtual table
	  ++t_cnt;
	  ++tt;
	}

      // data
      SET_VECTOR_ELT( c_list , cmd_cnt , t_list );
      // names 
      SET_STRING_ELT( c_names , cmd_cnt , Rf_mkChar( Helper::sanitize( cmd.name).c_str() ));

      // set all t_list names      
      Rf_setAttrib( t_list, R_NamesSymbol, t_names); 

      
      ++cmd_cnt;
      ++cc;
      
    }

  // set c_list names

  Rf_setAttrib( c_list, R_NamesSymbol, c_names); 

  UNPROTECT( protect );
  return c_list;
}
  


//
// extract raw signal data
//


SEXP Repoch_data( SEXP d , SEXP i )
{

  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }

  double dur = Rf_asReal( d );
  double inc = Rf_asReal( i );

  int ne = rdata->edf.timeline.set_epoch( dur , inc ); 
  
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(result)[0] = ne;
  UNPROTECT(1);

  return result;
  
}

SEXP Rextract_my_signals_by_epoch( SEXP e , SEXP chs )
{

  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }
  
  if ( ! rdata->edf.timeline.epoched() )
    Helper::halt( "study is not epoched" );

  int epoch = INTEGER(e)[0];

  int total_epochs = rdata->edf.timeline.num_total_epochs();
  
  // 1-based epoch count
  if ( epoch < 1 || epoch > total_epochs )
    Helper::halt( "invalid epoch number (expecting between 1 and"
		  + Helper::int2str( total_epochs ) + ")" );
  

  interval_t interval = rdata->edf.timeline.epoch( epoch );


  //
  // Which channels?
  //
  
  int nchs = Rf_length( chs ) ;

  signal_list_t signals;

  if ( nchs == 0 )
    {
      for (int s=0; s<rdata->edf.header.ns; s++) 
	signals.add( s, rdata->edf.header.label[s] );
    }
  else
    {
      for (int s=0; s<nchs; s++) 
	signals.add( s , CHAR( STRING_ELT( chs , s ) ) );
    }

  return Rextract_my_signals_internal( interval , signals );
  
}


SEXP Rextract_my_signals_internal( const interval_t & interval , const signal_list_t & signals )
{


  //
  // Get data 
  //



  const int ns = rdata->edf.header.ns;
  
  std::vector<int> Fs = rdata->edf.header.n_samples;
  
  int n_data_signals = 0;
  for (int s = 0 ; s < ns ; s++ )
    if ( ! rdata->edf.header.is_annotation_channel( s ) )
      ++n_data_signals;

  int protect = 0 ;
    
  SEXP r;
  PROTECT( r = Rf_allocVector( VECSXP, n_data_signals )); 
  ++protect;
  
  SEXP r_names;
  PROTECT( r_names = Rf_allocVector( STRSXP, n_data_signals )); 
  ++protect;
  
  int slot = 0;
  
  for (int s = 0 ; s < ns ; s++ )
    {

      if ( rdata->edf.header.is_annotation_channel( s ) )
	continue;
      
      slice_t slice( rdata->edf , s , interval );

      const std::vector<double> * d = slice.pdata();

      const int np = d->size();

      // channel name

      SET_STRING_ELT( r_names , slot ,
		      Rf_mkChar( rdata->edf.header.label[s].c_str() ) );
      
      // channel data
      
      SEXP sig;
      PROTECT( sig = Rf_allocVector( REALSXP, np )); 
      ++protect;
      
      for (int i=0;i<np;i++)
	REAL(sig)[i] = (*d)[i];

      SET_VECTOR_ELT( r , slot , sig );
      
      ++slot;
    }
  
  Rf_setAttrib( r , R_NamesSymbol, r_names); 

  // clean-up
  UNPROTECT( protect );
  
  return( r );
}



//
// apply a function over epochs
//

SEXP Riterate( SEXP fn , SEXP e , SEXP rho )
{

  if ( rdata == NULL )
    {
      R_error( "no EDF attached" );
      return( R_NilValue );
    }
  
  //
  // validate input
  //
  
  if ( ! Rf_isFunction(fn) ) 
    Helper::halt("'fn' must be a function");
  
  if ( ! Rf_isEnvironment(rho)) 
    Helper::halt("'rho' should be an environment");
  

//   // remember to check for user interupt on each iteration
//   // i.e. call R_CheckUserInterrupt();

//   void R_accumulate_func( Variant & v , void * p )
// {      
//   R_CheckUserInterrupt();
//   std::vector<Variant> * d = (std::vector<Variant>*)p;
//   d->push_back(v);
// }

// void R_iterate_func( Variant & v , void * p )
// {
//   R_CheckUserInterrupt();

//   Rdata * d = (Rdata*)p;
  
//   // Call R function, with variant and with result object
  
//   // Bind appropriate parameters
  
//   SEXP var;
  
//   PROTECT( var = Rvariant( v , *(d->opt) ) );
  
//   SETCADR( d->fncall, var );
  

//   // Evalute function

//   eval(d->fncall, d->rho);

  
//   UNPROTECT(1);    
  
// }

  
//   //
//   // Clean up
//   //

//   // UNPROTECT(1);
//   // etc


  return( R_NilValue );
}






// struct Rdata {
//   Rdisplay_options * opt;
//   SEXP fncall;
//   SEXP rho;
// };

// //
// // R iterate functions()
// //


// void R_accumulate_func( Variant & v , void * p )
// {      
//   R_CheckUserInterrupt();
//   std::vector<Variant> * d = (std::vector<Variant>*)p;
//   d->push_back(v);
// }

// void R_iterate_func( Variant & v , void * p )
// {
//   R_CheckUserInterrupt();

//   Rdata * d = (Rdata*)p;
  
//   // Call R function, with variant and with result object
  
//   // Bind appropriate parameters
  
//   SEXP var;
  
//   PROTECT( var = Rvariant( v , *(d->opt) ) );
  
//   SETCADR( d->fncall, var );
  

//   // Evalute function

//   eval(d->fncall, d->rho);

  
//   UNPROTECT(1);    
  
// }


// void R_group_iterate_func( VariantGroup & v , void * p )
// {
  
//   R_CheckUserInterrupt();

//   Rdata * d = (Rdata*)p;
  
//   // Call R function, with variant and with result object
  
//   // Bind appropriate parameters
  
//   SEXP var;
  
//   PROTECT( var = Rvariant_group( v , *(d->opt)) );
  
//   SETCADR( d->fncall, var );
  
//   // Evalute function
  
//   eval(d->fncall, d->rho);
  
//   UNPROTECT(1);    
  
// }


// SEXP R_getListElement(SEXP list, char *str)
// {
//   SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
//   for (int i = 0; i < length(list); i++)
//     if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
//       elmt = VECTOR_ELT(list, i);
//       break;
//     }
//   return elmt;
// }

// std::map<std::string,std::vector<std::string> > R_getListMap(SEXP list)
// {
//   std::map<std::string,std::vector<std::string> > m;
//   SEXP names = getAttrib(list, R_NamesSymbol);
//   for (int i = 0; i < length(list); i++)
//     { 
//       std::string key = CHAR(STRING_ELT(names, i));	
//       SEXP elmt = VECTOR_ELT(list, i);
//       for (int j = 0 ; j < length(elmt); j++ )
// 	m[key].push_back( CHAR(STRING_ELT(names, i)) );	
//     }
//   return m;    
// }

// std::set<std::string> R_getListNames(SEXP list)
// {
//   SEXP names = getAttrib(list, R_NamesSymbol);
//   std::set<std::string> n;
//   for (int i = 0; i < length(list); i++)
//     n.insert( CHAR(STRING_ELT(names, i)) );
//   return n;
// }






// SEXP Riterate(SEXP fn, SEXP rmask, SEXP ret, SEXP rho)
// {    
  
//   if ( ! R_project_attached ) { plog.warn( "no project attached" ); return( R_NilValue); } 
  
//   // 
//   // Do we want to accumulate and return the list of variants?
//   //
  
//   // ret = 0  implies we will iterate over a function, and not explicitly 
//   //          return any values
  
//   // ret > 0  implies we will rturn up to 'ret' values (i.e. make sure we do 
//   //          not explode memory in R
  
//   bool return_vars = false;
  
//   if ( length(ret)>0 && INTEGER(ret)[0] != 0 ) 
//     return_vars = true;
  
//   if ( ! return_vars )
//     {
//       if ( ! isFunction(fn) ) 
// 	Helper::halt("'fn' must be a function");
//     }
  
//   if ( ! isEnvironment(rho)) 
//     Helper::halt("'rho' should be an environment");
  


//   //
//   // Set-up R function call
//   //
  
//   SEXP R_fcall, ans;
  
//   PROTECT(R_fcall = lang2(fn, 
// 			  R_NilValue)); 
  
//   // Object to pass to C/C++ function, that points
//   // to the R function and an R object that can 
//   // collect any results


//   Rdisplay_options opt;
  
//   Rdata * d = new Rdata;
//   d->opt    = &opt;
//   d->fncall = R_fcall;
//   d->rho    = rho;

//   std::vector<Variant> vars;
//   std::vector<VariantGroup> varGroups;
  
//   //
//   // Construct mask
//   //
  
//   Mask mask;
  
//   if ( length(rmask) > 0 ) 
//     mask = R_make_mask(rmask);
  

//   if ( length(ret) > 0 && INTEGER(ret)[0] != 0 ) 
//     mask.limit( INTEGER(ret)[0]);
  
  

//   //
//   // Set up individual-map
//   //
  
//   gp->indmap.populate( gp->vardb , gp->phmap , mask );



//   //
//   // Iterate over all variants
//   //
  
//   if ( return_vars ) 
//     {
//       if ( mask.any_grouping() )	  
// 	gp->vardb.iterate( R_group_accumulate_func , &varGroups , mask );
//       else
// 	gp->vardb.iterate( R_accumulate_func , &vars , mask );
      
//     }
//   else if ( mask.any_grouping() )
//     {
//       gp->vardb.iterate( R_group_iterate_func , d , mask );
//     }
//   else
//     {
//       gp->vardb.iterate( R_iterate_func , d , mask );
//     }    
//   //
//   // Clean up...
//   //
  
//   UNPROTECT(1);        
//   delete d;  
  
  



  




SEXP Rmake_string_vector( std::vector<std::string> & r )
{
  SEXP v;
  PROTECT( v = Rf_allocVector( STRSXP, r.size()) );
  for (int i=0;i<r.size(); i++)
    SET_STRING_ELT(v, i, Rf_mkChar( r[i].c_str() ) );     
  UNPROTECT(1);
  return v;
}


std::vector<std::string> Rluna_to_strvector( SEXP x )
{
  const int n = Rf_length( x );
  std::vector<std::string> v(n);
  for (int i=0;i<n;i++) v[i] = CHAR( STRING_ELT( x, i ) );
  return v;
}


std::vector<int> Rluna_to_intvector( SEXP x )
{
  const int n = Rf_length( x );
  std::vector<int> v(n);
  if ( TYPEOF( x ) == INTSXP )
    for (int i=0;i<n;i++) v[i] = INTEGER(x)[i];
  return v;
}


std::vector<double> Rluna_to_dblvector( SEXP x )
{
  const int n = Rf_length( x );
  std::vector<double> v(n);
  if ( TYPEOF( x ) == REALSXP )
    for (int i=0;i<n;i++) v[i] = REAL(x)[i];
  return v;
}






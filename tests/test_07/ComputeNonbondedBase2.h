/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

EXCLUDED( FAST( foo bar ) )
EXCLUDED( MODIFIED( foo bar ) )
EXCLUDED( NORMAL( foo bar ) )
NORMAL( MODIFIED( foo bar ) )

    for (k=0; k<npairi; ++k) {

      const int j = pairlisti[k];
      register const CompAtom *p_j = p_1 + j;

      register const BigReal p_ij_x = p_i_x - p_j->position.x;
      register BigReal r2 = p_ij_x * p_ij_x;
      register const BigReal p_ij_y = p_i_y - p_j->position.y;
      r2 += p_ij_y * p_ij_y;
      register const BigReal p_ij_z = p_i_z - p_j->position.z;
      r2 += p_ij_z * p_ij_z;

      union { float f; int32 i; } r2f;
      r2f.f = r2;
      const int table_i = (r2f.i >> 17) + r2_delta_expc;

      FAST(
      const LJTable::TableEntry * lj_pars = 
		lj_row + 2 * mol->atomvdwtype(p_j->id) MODIFIED(+ 1);
      const BigReal* const vdwa_i = table_four + 16*table_i;
      BigReal vdwa_a = vdwa_i[0];
      const BigReal* const vdwb_i = table_four + 16*table_i + 4;
      BigReal vdwb_a = vdwb_i[0];
      SHORT(
      const BigReal* const fast_i = table_four + 16*table_i + 8;
      BigReal fast_a = fast_i[0];
      )
      )
      FULL(
      const BigReal* const scor_i = table_four + 16*table_i + 8 SHORT(+ 4);
      BigReal slow_a = scor_i[0]; 
      )

      r2f.i &= 0xfffe0000;

      /*
      BigReal modf = 0.0;
      int atom2 = p_j->id;
      register char excl_flag = ( (atom2 >= excl_min && atom2 <= excl_max) ?
					excl_flags[atom2-excl_min] : 0 );
      if ( excl_flag ) { ++exclChecksum; }
      SELF( if ( j < j_hgroup ) { excl_flag = EXCHCK_FULL; } )
      if ( excl_flag ) {
	if ( excl_flag == EXCHCK_FULL ) {
	  lj_pars = lj_null_pars;
	  modf = 1.0;
	} else {
	  ++lj_pars;
	  modf = modf_mod;
	}
      }
      */

      BigReal kqq = kq_i * p_j->charge;
      const BigReal diffa = r2 - r2f.f;

      FEP(
      int jfep_type = p_j->partition;
      BigReal lambda_pair = lambda_table_i[2*jfep_type];
      BigReal d_lambda_pair = lambda_table_i[2*jfep_type+1];
      )

      LES( BigReal lambda_pair = lambda_table_i[p_j->partition]; )

      FAST
      (
      const BigReal A = scaling * lj_pars->A;
      const BigReal B = scaling * lj_pars->B;

      BigReal vdw_a = A * vdwa_a - B * vdwb_a;
      BigReal vdw_d = A * vdwa_i[3] - B * vdwb_i[3];
      BigReal vdw_c = A * vdwa_i[2] - B * vdwb_i[2];
      BigReal vdw_b = A * vdwa_i[1] - B * vdwb_i[1];

      ENERGY(
      register BigReal vdw_val =
        ( ( diffa * vdw_d + vdw_c ) * diffa + vdw_b ) * diffa + vdw_a;
      vdwEnergy += LAM(lambda_pair *) vdw_val;
      FEP( vdwEnergy_s += d_lambda_pair * vdw_val; )
      )

      INT( 
      register BigReal vdw_dir =
	( 3.0 * diffa * vdw_d + 2.0 * vdw_c ) * diffa + vdw_b;
      // BigReal force_r = -1.0 * LAM(lambda_pair *) vdw_dir;
      reduction[pairVDWForceIndex_X] -= 2.0 * vdw_dir * p_ij_x;
      reduction[pairVDWForceIndex_Y] -= 2.0 * vdw_dir * p_ij_y;
      reduction[pairVDWForceIndex_Z] -= 2.0 * vdw_dir * p_ij_z;
      )

      SHORT(
      NORMAL(
      fast_a *= kqq;
      BigReal fast_d = kqq * fast_i[3];
      BigReal fast_c = kqq * fast_i[2];
      BigReal fast_b = kqq * fast_i[1];
      )
      MODIFIED(
      BigReal modfckqq = (1.0-modf_mod) * kqq;
      fast_a *= modfckqq;
      BigReal fast_d = modfckqq * fast_i[3];
      BigReal fast_c = modfckqq * fast_i[2];
      BigReal fast_b = modfckqq * fast_i[1];
      )

      {
      ENERGY(
      register BigReal fast_val =
	( ( diffa * fast_d + fast_c ) * diffa + fast_b ) * diffa + fast_a;
      electEnergy += LAM(lambda_pair *) fast_val;
      FEP( electEnergy_s += d_lambda_pair * fast_val; )
      )

      INT(
      register BigReal fast_dir =
	( 3.0 * diffa * fast_d + 2.0 * fast_c ) * diffa + fast_b;
      // force_r -= LAM(lambda_pair *) fast_dir;
      reduction[pairElectForceIndex_X] -= 2.0 * fast_dir * p_ij_x;
      reduction[pairElectForceIndex_Y] -= 2.0 * fast_dir * p_ij_y;
      reduction[pairElectForceIndex_Z] -= 2.0 * fast_dir * p_ij_z;
      )
      }

      fast_d += vdw_d;
      fast_c += vdw_c;
      fast_b += vdw_b;
      fast_a += vdw_a;
      register BigReal fast_dir =
	( 3.0 * diffa * fast_d + 2.0 * fast_c ) * diffa + fast_b;
      BigReal force_r = -2.0 * LAM(lambda_pair *) fast_dir;
      Force & f_j = f_1[j];
      register BigReal tmp_x = force_r * p_ij_x;
      virial_xx += tmp_x * p_ij_x;
      virial_xy += tmp_x * p_ij_y;
      virial_xz += tmp_x * p_ij_z;
      f_i.x += tmp_x;
      f_j.x -= tmp_x;
      register BigReal tmp_y = force_r * p_ij_y;
      virial_yy += tmp_y * p_ij_y;
      virial_yz += tmp_y * p_ij_z;
      f_i.y += tmp_y;
      f_j.y -= tmp_y;
      register BigReal tmp_z = force_r * p_ij_z;
      virial_zz += tmp_z * p_ij_z;
      f_i.z += tmp_z;
      f_j.z -= tmp_z;

      INT(
      if (pressureProfileNonbonded) {
        const BigReal p_j_z = p_j->position.z;
        int n1 = (int)floor((p_i_z-pressureProfileMin)/pressureProfileThickness);
        int n2 = (int)floor((p_j_z-pressureProfileMin)/pressureProfileThickness);
        pp_reduction(pressureProfileThickness, pressureProfileMin,
                     pressureProfileSlabs, p_i_z, p_j_z, n1, n2, 
                     tmp_x*p_ij_x, tmp_y * p_ij_y, tmp_z*p_ij_z,
                     pressureProfileReduction);

      } 
      )

      )
      )

      FULL(
      BigReal slow_b = scor_i[1];
      BigReal slow_c = scor_i[2];
      BigReal slow_d = scor_i[3];
      EXCLUDED(
      const BigReal* const slow_i
		SHORT( = slow_table + 4*table_i; )
		NOSHORT( = table_four + 12 + 16*table_i; )
      slow_a -= slow_i[0];
      slow_b -= slow_i[1];
      slow_c -= slow_i[2];
      slow_d -= slow_i[3];
      )
      MODIFIED(
      const BigReal* const slow_i
		SHORT( = slow_table + 4*table_i; )
		NOSHORT( = table_four + 12 + 16*table_i; )
      slow_a -= modf_mod * slow_i[0];
      slow_b -= modf_mod * slow_i[1];
      slow_c -= modf_mod * slow_i[2];
      slow_d -= modf_mod * slow_i[3];
      )
      slow_d *= kqq;
      slow_c *= kqq;
      slow_b *= kqq;
      slow_a *= kqq;

      ENERGY(
      register BigReal slow_val =
	( ( diffa * slow_d + slow_c ) * diffa + slow_b ) * diffa + slow_a;
      fullElectEnergy += LAM(lambda_pair *) slow_val;
      FEP( fullElectEnergy_s += d_lambda_pair * slow_val; )
      )

      INT( {
      register BigReal slow_dir =
	( 3.0 * diffa * slow_d + 2.0 * slow_c ) * diffa + slow_b;
      reduction[pairElectForceIndex_X] -= 2.0 * slow_dir * p_ij_x;
      reduction[pairElectForceIndex_Y] -= 2.0 * slow_dir * p_ij_y;
      reduction[pairElectForceIndex_Z] -= 2.0 * slow_dir * p_ij_z;
      } )

      FAST(
      NOSHORT(
      slow_d += vdw_d;
      slow_c += vdw_c;
      slow_b += vdw_b;
      slow_a += vdw_a;
      )
      )

      register BigReal slow_dir =
	( 3.0 * diffa * slow_d + 2.0 * slow_c ) * diffa + slow_b;
      BigReal fullforce_r = -2.0 * slow_dir LAM(* lambda_pair);

      {
      Force & fullf_j = fullf_1[j];
      register BigReal tmp_x = fullforce_r * p_ij_x;
      fullElectVirial_xx += tmp_x * p_ij_x;
      fullElectVirial_xy += tmp_x * p_ij_y;
      fullElectVirial_xz += tmp_x * p_ij_z;
      fullf_i.x += tmp_x;
      fullf_j.x -= tmp_x;
      register BigReal tmp_y = fullforce_r * p_ij_y;
      fullElectVirial_yy += tmp_y * p_ij_y;
      fullElectVirial_yz += tmp_y * p_ij_z;
      fullf_i.y += tmp_y;
      fullf_j.y -= tmp_y;
      register BigReal tmp_z = fullforce_r * p_ij_z;
      fullElectVirial_zz += tmp_z * p_ij_z;
      fullf_i.z += tmp_z;
      fullf_j.z -= tmp_z;

      }
      )

    } // for pairlist


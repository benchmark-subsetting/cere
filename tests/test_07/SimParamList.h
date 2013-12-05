/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

	SIMPARAM(Vector,cellBasisVector1,Vector(0,0,0));	//  Used to build lattice object
	SIMPARAM(Vector,cellBasisVector2,Vector(0,0,0));
	SIMPARAM(Vector,cellBasisVector3,Vector(0,0,0));
	SIMPARAM(Vector,cellOrigin,Vector(0,0,0));
	
	SIMPARAM(int,nonbondedFrequency,0);		//  Number of timesteps between
					//  nonbonded evaluation
	SIMPARAM(int,fullElectFrequency,0);		//  Number of timesteps between
					//  full electrostatic evaluation
	SIMPARAM(BigReal,dielectric,0);   		//  Dielectric constant
	SIMPARAM(ExclusionSettings,exclude,0);	//  What electrostatic exclusions should
					//  be made
	SIMPARAM(BigReal,scale14,0);		//  Scaling factor for 1-4 
					//  electrostatics
	SIMPARAM(BigReal,nonbondedScaling,0);	//  Scaling factor for nonbonded forces
	SIMPARAM(BigReal,cutoff,0);			//  Cutoff distance
	SIMPARAM(Bool,switchingActive,0);		//  Flag TRUE->using switching function
					//  for electrostatics and vdw
	SIMPARAM(BigReal,switchingDist,0);		//  Distance at which switching
					//  becomes active
	SIMPARAM(BigReal,pairlistDist,0);		//  Distance within which atom pairs 
					//  should be added to pairlist

//Modifications for alchemical fep
//SD & CC, CNRS - LCTN, Nancy
//   Begin FEP flags
	SIMPARAM(Bool,fepOn,0);			//  Doing alchemical FEP?
	SIMPARAM(BigReal,lambda,0);			//  lambda for dynamics
	SIMPARAM(BigReal,lambda2,0);		//  lambda for comparison
	SIMPARAM(int,fepOutFreq,0);			//  freq of fep output
	SIMPARAM(int,fepEquilSteps,0);		//  no of eqlb steps in the window
//   End FEP flags
//fepe

        SIMPARAM(Bool,pressureProfileOn,0);         // Compute lateral pressure profile?
        SIMPARAM(Bool,pressureProfileNonbonded,0);  // Compute only nonbonded contribution?
        SIMPARAM(int,pressureProfileSlabs,0);       // Number of slabs
        SIMPARAM(int,pressureProfileFreq,0);        // How often to store profile data
        // rest of pp params are computed from the previous.
        SIMPARAM(BigReal,pressureProfileMin,0);     // coordinate of bottom of lowest slab
        SIMPARAM(BigReal,pressureProfileThickness,0);  // thickness of a slab

	SIMPARAM(Bool,lesOn,0);			//  Locally enhanced sampling?
	SIMPARAM(int,lesFactor,0);			//  local enhancement factor

	SIMPARAM(Bool,pairInteractionOn,0);		//  Calculate pair interactions?
	SIMPARAM(int,pairInteractionGroup1,0);	//  Interaction group 1.
	SIMPARAM(int,pairInteractionGroup2,0);	//  Interaction group 2.
        SIMPARAM(Bool,pairInteractionSelf,0);       //  Compute just within group.
     
	SIMPARAM(Bool,fixedAtomsOn,0);		//  Are there fixed atoms?
	SIMPARAM(Bool,fixedAtomsForces,0);		//  Calculate forces anyway?

	SIMPARAM(Bool,FMAOn,0);			//  Flag TRUE-> FMA active
	SIMPARAM(Bool,fullDirectOn,0);		//  Should direct calculations of
	SIMPARAM(Bool,PMEOn,0);			//  Flag TRUE -> PME active
	SIMPARAM(BigReal,PMETolerance,0);		//  Direct space tolerance
        SIMPARAM(BigReal,PMEEwaldCoefficient,0);    //  From tolerance and cutoff

	SIMPARAM(int,longSplitting,0);		//  What electrostatic splitting 	
					//  to use

	SIMPARAM(BigReal,hgroupCutoff,0);		// what is the added hydrogen margin?

	SIMPARAM(int,mollyOn,0);			// mollify long range forces?

	SIMPARAM(Bool,commOnly,0);			//  Don't do any force evaluations


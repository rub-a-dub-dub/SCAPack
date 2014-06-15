
BeginPackage["SCAPack`"]

(* ::Section:: *)
(* Public Messages *)

NetlistCircuit::usage = 
  "This function accepts a netlist in SCA format and creates nodal equations \
for the netlist.";

GetSolveVariables::usage = 
  "This function determines all of the solver variables in the netlist. Use \
this with Solve[].";

SolveFullSystem::usage = 
  "This function will automatically solve a given netlist for all identified \
variables.";

GetTransferFunction::usage = 
  "This function will solve the transfer function for the given netlist from \
the input node to the output node.";

GetNoisePSD::usage = 
  "This function will solve the noise transfer function for the netlist and a \
given output node and noise source name.";

GetTotalNoisePSD::usage = 
  "This function will determine the transfer function for the noise at an \
output node as a function of the noise for each noise source.";

GetInputImpedance::usage = 
  "This function will return the input impedance for the specified nodes.";

GetZVTimeConstants::usage = 
  "This function will compute the zero-value time constant (aka OCTCA) \
approximate transfer function.";

DisableSources::usage = "This function will disable independent sources in the \
netlist according to a user specified rule.";

ComputeZNetwork::usage = "This function computes the Z-parameter network representation \
of a netlist.";

ComputeYNetwork::usage = "This function computes the Y-parameter network representation \
of a netlist.";

ComputeHNetwork::usage = "This function computes the H-parameter network representation \
of a netlist.";

ComputeGNetwork::usage = "This function computes the G-parameter network representation \
of a netlist.";

ComputeABCDNetwork::usage = "This function computes the ABCD-parameter network representation of a netlist.";

ComputeSNetwork::usage = "This function computes the S-parameter network representation of a netlist.";

ComputeTNetwork::usage = "This function computes the T-parameter network representation of a netlist.";

NetworkData::usage = "This function represents raw data for network parameter analyses.";

LosslessQ::usage = "This function determines if a network is lossless.";
ReciprocalQ::usage = "This function determines if a network is reciprocal.";
SymmetricQ::usage = "This function determines if a network is symmetric.";

NetworkProperties::netwdata = "Expecting a NetworkData object.";

(* ::Section:: *)
(* Private Messages *)

Begin["`Private`"];

ErrorCheckNetlist::dupenames = 
  "The following element names have been duplicated: `1`.";
ErrorCheckNetlist::oneconn = 
  "The following nodes have only one connection: `1`.";
ErrorCheckNetlist::shortedelement = "The element `1` has been shorted out.";
ErrorCheckNetlist::incompletenetlist = 
  "The netlist provided seems incomplete - a Null item was encountered. There \
may be an extra comma within the netlist.";
ErrorCheckNetlist::invalidnodename = 
  "The node `1` is neither a symbol or ground (0).";
ErrorCheckNetlist::prbnotfound = 
  "The current probe `1` was not found in the netlist. Remember that element names are case sensitive.";
ErrorCheckNetlist::invalidline = "The netlist line `1` is badly formed.";
ErrorCheckNetlist::invalidparam = "The parameter `1` is unknown.";
ErrorCheckNetlist::invalidnetlist = "The netlist is invalid.";
ErrorCheckNetlist::wrongnodes = "The line `1` has the incorrect number of node connections.";
ErrorCheckNetlist::reqparam = "The needed parameter `1` is missing from the netlist.";
ErrorCheckNetlist::nestedsubckt = "Subcircuits may not be nested.";
ErrorCheckNetlist::nodenotfound = "The node `1` referenced is not found in the netlist.";
ErrorCheckNetlist::doublepars = "Both resistance and conductance have been specified for `1`.";
DisableSources::invalidchglist = "The rule to disable a source (`1`) is invalid.";
DisableSources::nochglist = "No source disabling rules were speecified.";
DisableSources::invalidsrc = "The source referenced `1` was not found. Is it a voltage/current source?";
GetZVTimeConstants::indsrcfnd = "ZVTCA can only be used on networks without indepedent sources.";
GetInputImpedance::indsrcfnd = GetZVTimeConstants::indsrcfnd;
ComputeNetwork::indsrcfnd = GetZVTimeConstants::indsrcfnd;
ComputeNetwork::twoports = "This analyses is only applicable for 2-port networks.";
ComputeNetwork::portnotfound = "One or more of the ports specified was not found in the netlist.";
NetlistCircuit::unknownelem = "An unknown element `1` has been encountered.";
NetlistCircuit::termerror = 
  "An internal netlisting error occurred due to a terminal identification \
error.";
GetNoisePSD::srcnotfound = 
  "The noise source `1` was not found in the netlist.";
GetTransferFunction::nodenotfound = 
  "The node `1` was not found in the netlist.";
GetInputImpedance::nodenotfound = 
  "The node `1` was not found in the netlist.";
NetworkData::nodatafound = "The data for parameter \"`1`\" was not found. Valid options are Z, Y, G and H.";
NetworkData::twopars = "Multiple network parameter datasets are available. Specify them explicitly as an additional parameter.";
NetworkData::unknfmt = "The parameter type `1` is unknown. Supported types are Z, Y, G, H and ABCD.";
NetworkData::baddata = "The data provided is not correctly formed for the type you have specified. Are the number of ports correct and is the matrix formed correctly?";

(* ::Section:: *)
(* Device Models *)

MOSModelResistances[Vd_, Vgp_, Vs_, Vb_, name_, Noisy_:False] := 
  Block[{Ig, Id, Ib, Is, myCgs = Global`Cgs, myCgd = Global`Cgd, 
    myS = Global`s, myGm = Global`Gm, myGds = Global`Gds, myCds = Global`Cds, 
    myRg = Global`Rg, myRd = Global`Rd, myRs = Global`Rs, myCdb = Global`Cdb, 
    myGmb = Global`Gmb, myCsb = Global`Csb, myCgb = Global`Cgb, 
    myRb = Global`Rb, Vg, mySirs, mySird, mySirg, mySirb, mySid},
    If[ Noisy === True,
    	Vg = Vgp + ToExpression["Svg$"<>name];
    	mySirs = ToExpression["Sirs$"<>name];
    	mySird = ToExpression["Sird$"<>name];
    	mySirg = ToExpression["Sirg$"<>name];
    	mySirb = ToExpression["Sirb$"<>name];
    	mySid = ToExpression["Sid$"<>name];
    	,
    	Vg = Vgp;
    	mySirs = 0;
    	mySird = 0;
    	mySirg = 0;
    	mySirb = 0;
    	mySid = 0;
    ];
   Ib = (myS (myCgb ((1 + 
           myGds myRd + (myGds + myGm + myGmb) myRs) (myRb mySirb - 
           myRg mySirg + Vb - Vg) + 
        myCgd myS (myRg (1 + 
              myRs (myGds + myGm + myGmb + myCgs myS)) (myRb mySirb + 
              Vb - Vd) + 
           myRd ((1 + 
                 myRs (myGds + myGm + myGmb + 
                    myCgs myS)) (myRb mySirb + Vb - Vg) + 
              myRg (mySid - mySird - 
                 mySirg + (myGds + myGm + myGmb + 
                    myCgs myS) (myRb mySirb - 
                    myRs (mySird + mySirg + mySirs) + Vb - Vs)))) - 
        myCgs myS (-(1 + myGds myRd) myRs (myRb mySirb + Vb - Vg) + 
           myRg (myRs (mySid + mySirg + mySirs + 
                 myGds (-myRb mySirb + 
                    myRd (mySird + mySirg + mySirs) - Vb + Vd)) - (1 +
                  myGds myRd) (myRb mySirb + Vb - Vs)))) - 
     myCsb (myRs ((1 + (myCgb + myCgs) myRg myS + 
              myCgd (myRd + myRg) myS) mySid - myGm myRb mySirb - 
           myCgb myRb myS mySirb - myCgs myRb myS mySirb - 
           myCgd myGm myRb myRd myS mySirb - 
           myCgd myGm myRb myRg myS mySirb - 
           myCgb myCgd myRb myRd myS^2 mySirb - 
           myCgd myCgs myRb myRd myS^2 mySirb - 
           myCgb myCgd myRb myRg myS^2 mySirb - 
           myCgd myCgs myRb myRg myS^2 mySirb + 
           myCgd myGm myRd myRg myS mySird + 
           myCgb myCgd myRd myRg myS^2 mySird + 
           myCgd myCgs myRd myRg myS^2 mySird + myGm myRg mySirg + 
           myCgb myRg myS mySirg + myCgs myRg myS mySirg + 
           myCgd myGm myRd myRg myS mySirg + 
           myCgb myCgd myRd myRg myS^2 mySirg + 
           myCgd myCgs myRd myRg myS^2 mySirg + mySirs + 
           myCgd myRd myS mySirs + myCgb myRg myS mySirs + 
           myCgd myRg myS mySirs + myCgs myRg myS mySirs + 
           myCgd myGm myRd myRg myS mySirs + 
           myCgb myCgd myRd myRg myS^2 mySirs + 
           myCgd myCgs myRd myRg myS^2 mySirs - myGm Vb - 
           myCgb myS Vb - myCgs myS Vb - myCgd myGm myRd myS Vb - 
           myCgd myGm myRg myS Vb - myCgb myCgd myRd myS^2 Vb - 
           myCgd myCgs myRd myS^2 Vb - myCgb myCgd myRg myS^2 Vb - 
           myCgd myCgs myRg myS^2 Vb + myCgd myGm myRg myS Vd + 
           myCgb myCgd myRg myS^2 Vd + 
           myCgd myCgs myRg myS^2 Vd + (myGm + (myCgb + 
                 myCgs) myS) (1 + myCgd myRd myS) Vg + 
           myGds (-myRb (1 + (myCgb + myCgd + myCgs) (myRd + 
                    myRg) myS) mySirb - (1 + (myCgb + myCgd + 
                    myCgs) myRg myS) (Vb - Vd) + 
              myRd ((1 + (myCgb + myCgd + myCgs) myRg myS) mySird + 
                 mySirs + (myCgb + myCgd + 
                    myCgs) myS (myRg (mySirg + mySirs) - Vb + 
                    Vg)))) - (1 + (myCgb + myCgs) myRg myS + 
           myGds myRd (1 + (myCgb + myCgd + myCgs) myRg myS) + 
           myCgd myS (myRd + myRg + 
              myGm myRd myRg + (myCgb + 
                 myCgs) myRd myRg myS)) (myRb mySirb + Vb - Vs)) + 
     myCdb ((1 + (myCgb + myCgd + myCgs) myRg myS + 
           myRs (myGds + myGm + 
              myGmb + (myCgs + myCsb + 
                 myCgs (myGds + myGmb) myRg + (myCgb + myCgd) (myGds +
                     myGm + myGmb) myRg) myS + ((myCgb + 
                    myCgd) myCgs + (myCgb + myCgd + 
                    myCgs) myCsb) myRg myS^2)) (myRb mySirb + Vb - 
           Vd) + myRd ((1 + ((myCgb + myCgd + myCgs) myRg + 
                 myCgs myRs) myS) mySid + (myGds + 
              myGmb) myRb mySirb + myCgd myRb myS mySirb + 
           myCsb myRb myS mySirb + myCgd myGds myRb myRg myS mySirb + 
           myCgs myGds myRb myRg myS mySirb + 
           myCgd myGm myRb myRg myS mySirb + 
           myCgd myGmb myRb myRg myS mySirb + 
           myCgs myGmb myRb myRg myS mySirb + 
           myCgd myGds myRb myRs myS mySirb + 
           myCgs myGds myRb myRs myS mySirb + 
           myCgd myGm myRb myRs myS mySirb + 
           myCgd myGmb myRb myRs myS mySirb + 
           myCgs myGmb myRb myRs myS mySirb + 
           myCgd myCgs myRb myRg myS^2 mySirb + 
           myCgd myCsb myRb myRg myS^2 mySirb + 
           myCgs myCsb myRb myRg myS^2 mySirb + 
           myCgd myCgs myRb myRs myS^2 mySirb + 
           myCgd myCsb myRb myRs myS^2 mySirb + 
           myCgs myCsb myRb myRs myS^2 mySirb - mySird - 
           myGds myRs mySird - myGm myRs mySird - myGmb myRs mySird - 
           myCgd myRg myS mySird - myCgs myRg myS mySird - 
           myCgs myRs myS mySird - myCsb myRs myS mySird - 
           myCgd myGds myRg myRs myS mySird - 
           myCgs myGds myRg myRs myS mySird - 
           myCgd myGm myRg myRs myS mySird - 
           myCgd myGmb myRg myRs myS mySird - 
           myCgs myGmb myRg myRs myS mySird - 
           myCgd myCgs myRg myRs myS^2 mySird - 
           myCgd myCsb myRg myRs myS^2 mySird - 
           myCgs myCsb myRg myRs myS^2 mySird + myGm myRg mySirg - 
           myCgd myRg myS mySirg - myCgd myGds myRg myRs myS mySirg - 
           myCgs myGds myRg myRs myS mySirg - 
           myCgd myGm myRg myRs myS mySirg - 
           myCgd myGmb myRg myRs myS mySirg - 
           myCgs myGmb myRg myRs myS mySirg - 
           myCgd myCgs myRg myRs myS^2 mySirg - 
           myCgd myCsb myRg myRs myS^2 mySirg - 
           myCgs myCsb myRg myRs myS^2 mySirg - myGds myRs mySirs - 
           myGm myRs mySirs - myGmb myRs mySirs - 
           myCsb myRs myS mySirs - myCgd myGds myRg myRs myS mySirs - 
           myCgs myGds myRg myRs myS mySirs - 
           myCgd myGm myRg myRs myS mySirs - 
           myCgd myGmb myRg myRs myS mySirs - 
           myCgs myGmb myRg myRs myS mySirs - 
           myCgd myCgs myRg myRs myS^2 mySirs - 
           myCgd myCsb myRg myRs myS^2 mySirs - 
           myCgs myCsb myRg myRs myS^2 mySirs + myGds Vb + myGmb Vb + 
           myCgd myS Vb + myCsb myS Vb + myCgd myGds myRg myS Vb + 
           myCgs myGds myRg myS Vb + myCgd myGm myRg myS Vb + 
           myCgd myGmb myRg myS Vb + myCgs myGmb myRg myS Vb + 
           myCgd myGds myRs myS Vb + myCgs myGds myRs myS Vb + 
           myCgd myGm myRs myS Vb + myCgd myGmb myRs myS Vb + 
           myCgs myGmb myRs myS Vb + myCgd myCgs myRg myS^2 Vb + 
           myCgd myCsb myRg myS^2 Vb + myCgs myCsb myRg myS^2 Vb + 
           myCgd myCgs myRs myS^2 Vb + myCgd myCsb myRs myS^2 Vb + 
           myCgs myCsb myRs myS^2 Vb + myGm Vg - myCgd myS Vg - 
           myCgd myGds myRs myS Vg - myCgs myGds myRs myS Vg - 
           myCgd myGm myRs myS Vg - myCgd myGmb myRs myS Vg - 
           myCgs myGmb myRs myS Vg - myCgd myCgs myRs myS^2 Vg - 
           myCgd myCsb myRs myS^2 Vg - 
           myCgs myCsb myRs myS^2 Vg - (myGds + myGm + myGmb + 
              myCsb myS + (myCgd + myCgs) myGds myRg myS + 
              myRg myS (myCgs (myGmb + myCsb myS) + 
                 myCgd (myGm + myGmb + (myCgs + myCsb) myS))) Vs + 
           myCgb myS (myRb (1 + (myRg + myRs) (myGds + myGm + 
                    myGmb + (myCgs + myCsb) myS)) mySirb + (1 + 
                 myRs (myGds + myGm + 
                    myGmb + (myCgs + myCsb) myS)) (Vb - Vg) - 
              myRg ((1 + 
                    myRs (myGds + myGm + 
                    myGmb + (myCgs + myCsb) myS)) mySird + 
                 mySirg + (myGds + myGm + 
                    myGmb + (myCgs + myCsb) myS) (myRs (mySirg + 
                    mySirs) - Vb + Vs)))))))/(1 + (myGm + 
      myGmb) myRs + (myCdb + myCgb + myCsb) myRb myS + (myCdb myRd + 
      myCgd myRd + myCdb myGmb myRb myRd + myCgb myRg + myCgd myRg + 
      myCgs myRg + 
      myCgd myGm myRd myRg + (myCgs + myCsb + myCsb myGm myRb + 
         myCgs myGmb myRg + (myGm + myGmb) (myCdb (myRb + myRd) + 
            myCgb (myRb + myRg) + 
            myCgd (myRd + myRg))) myRs) myS + (myCgd myCsb myRb myRd +
       myCgd myCsb myRb myRg + myCgs myCsb myRb myRg + 
      myCgd myCgs myRd myRg + 
      myCgd myCsb myGm myRb myRd myRg + (myCgs (myCsb (myRb + myRg) + 
            myCgd (myRd + myRg)) + 
         myCgd myCsb (myRg + myGm myRb myRg + 
            myRd (1 + myGm (myRb + myRg)))) myRs + 
      myCgb ((myCgs + myCsb) (myRg myRs + myRb (myRg + myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs))) + 
      myCdb (myCsb myRb myRd + myCgs myRb myRg + myCgs myRd myRg + 
         myCgs myGmb myRb myRd myRg + (myCsb (myRb + myRd) + 
            
            myCgs (myRb + myRd + myGmb myRb myRd + 
               myGmb (myRb + myRd) myRg)) myRs + 
         myCgb (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + 
                  myRg) myRs)))) myS^2 + (myCdb (myCgb + 
         myCgd) myCgs + myCgd myCgs myCsb + 
      myCdb (myCgb + myCgd + myCgs) myCsb + 
      myCgb myCgd (myCgs + myCsb)) (myRb myRd myRg + myRd myRg myRs + 
      myRb (myRd + myRg) myRs) myS^3 + 
   myGds (myRd + 
      myRs + ((myCdb + myCgb + myCsb) myRb myRd + (myCgb + myCgd + 
            myCgs) myRd myRg + (myCdb (myRb + myRd) + 
            myCsb (myRb + myRd) + 
            myCgb (myRb + myRg) + (myCgd + myCgs) (myRd + 
               myRg)) myRs) myS + (myCdb (myCgb + myCgd + 
            myCgs) + (myCgd + myCgs) myCsb + 
         myCgb (myCgd + myCgs + myCsb)) (myRb myRd myRg + 
         myRd myRg myRs + myRb (myRd + myRg) myRs) myS^2));
   Ig = (myS (-myCgs (myRs ((1 + (myCgb + myCsb) myRb myS + 
              myCdb (myRb + myRd) myS) mySid + myGmb myRb mySirb + 
           myCgb myRb myS mySirb + myCsb myRb myS mySirb + 
           myCdb myGds myRb myRd myS mySirb + 
           myCgb myGds myRb myRd myS mySirb + 
           myCsb myGds myRb myRd myS mySirb + 
           myCdb myGmb myRb myRd myS mySirb + 
           myCdb myCgb myRb myRd myS^2 mySirb + 
           myCdb myCsb myRb myRd myS^2 mySirb + myGds myRd mySird + 
           myCdb myGds myRb myRd myS mySird + 
           myCgb myGds myRb myRd myS mySird + 
           myCsb myGds myRb myRd myS mySird + 
           myCdb myGmb myRb myRd myS mySird + 
           myCdb myCgb myRb myRd myS^2 mySird + 
           myCdb myCsb myRb myRd myS^2 mySird - myGds myRg mySirg - 
           myGmb myRg mySirg - myCgb myRg myS mySirg - 
           myCsb myRg myS mySirg - myCdb myGds myRb myRg myS mySirg - 
           myCgb myGds myRb myRg myS mySirg - 
           myCsb myGds myRb myRg myS mySirg - 
           myCdb myGmb myRb myRg myS mySirg - 
           myCdb myGds myRd myRg myS mySirg - 
           myCgb myGds myRd myRg myS mySirg - 
           myCsb myGds myRd myRg myS mySirg - 
           myCdb myGmb myRd myRg myS mySirg - 
           myCdb myCgb myRb myRg myS^2 mySirg - 
           myCdb myCsb myRb myRg myS^2 mySirg - 
           myCdb myCgb myRd myRg myS^2 mySirg - 
           myCdb myCsb myRd myRg myS^2 mySirg + mySirs + 
           myGds myRd mySirs + myCdb myRb myS mySirs + 
           myCgb myRb myS mySirs + myCsb myRb myS mySirs + 
           myCdb myRd myS mySirs + myCdb myGds myRb myRd myS mySirs + 
           myCgb myGds myRb myRd myS mySirs + 
           myCsb myGds myRb myRd myS mySirs + 
           myCdb myGmb myRb myRd myS mySirs + 
           myCdb myCgb myRb myRd myS^2 mySirs + 
           myCdb myCsb myRb myRd myS^2 mySirs + myGmb Vb + 
           myCgb myS Vb + myCsb myS Vb + myCdb myGds myRd myS Vb + 
           myCgb myGds myRd myS Vb + myCsb myGds myRd myS Vb + 
           myCdb myGmb myRd myS Vb + myCdb myCgb myRd myS^2 Vb + 
           myCdb myCsb myRd myS^2 Vb + myGds Vd + 
           myCdb myGds myRb myS Vd + myCgb myGds myRb myS Vd + 
           myCsb myGds myRb myS Vd + myCdb myGmb myRb myS Vd + 
           myCdb myCgb myRb myS^2 Vd + 
           myCdb myCsb myRb myS^2 Vd - (myGds + (myCdb + myCgb + 
                 myCsb) myGds (myRb + 
                 myRd) myS + (myGmb + (myCgb + myCsb) myS) (1 + 
                 myCdb (myRb + myRd) myS)) Vg) - (1 + (myCdb + myCgb +
               myCsb) myRb myS + 
           myRd (myGds + (myCdb + myCgb + myCsb) myGds myRb myS + 
              myCdb myS (1 + 
                 myGmb myRb + (myCgb + 
                    myCsb) myRb myS))) (myRg mySirg + Vg - Vs)) + 
     myCgd ((1 + (myCdb + myCgb + myCsb) myRb myS + 
           myRs (myGds + myGm + 
              myGmb + (myCgs + myCsb + 
                 myCsb (myGds + myGm) myRb + (myCdb + myCgb) (myGds + 
                    myGm + myGmb) myRb) myS + ((myCdb + 
                    myCgb) myCgs + (myCdb + myCgb + 
                    myCgs) myCsb) myRb myS^2)) (myRg mySirg - Vd + 
           Vg) + myRd ((1 + ((myCdb + myCgb + myCsb) myRb + 
                 myCsb myRs) myS) mySid - myCdb myRb myS mySirb - 
           myCgb myRb myS mySirb - myCdb myGds myRb myRs myS mySirb - 
           myCgb myGds myRb myRs myS mySirb - 
           myCsb myGds myRb myRs myS mySirb - 
           myCdb myGm myRb myRs myS mySirb - 
           myCgb myGm myRb myRs myS mySirb - 
           myCsb myGm myRb myRs myS mySirb - 
           myCdb myCgs myRb myRs myS^2 mySirb - 
           myCgb myCgs myRb myRs myS^2 mySirb - 
           myCdb myCsb myRb myRs myS^2 mySirb - 
           myCgb myCsb myRb myRs myS^2 mySirb - 
           myCgs myCsb myRb myRs myS^2 mySirb - mySird - 
           myGds myRs mySird - myGm myRs mySird - 
           myCdb myRb myS mySird - myCgb myRb myS mySird - 
           myCsb myRb myS mySird - myCgs myRs myS mySird - 
           myCsb myRs myS mySird - myCdb myGds myRb myRs myS mySird - 
           myCgb myGds myRb myRs myS mySird - 
           myCsb myGds myRb myRs myS mySird - 
           myCdb myGm myRb myRs myS mySird - 
           myCgb myGm myRb myRs myS mySird - 
           myCsb myGm myRb myRs myS mySird - 
           myCdb myCgs myRb myRs myS^2 mySird - 
           myCgb myCgs myRb myRs myS^2 mySird - 
           myCdb myCsb myRb myRs myS^2 mySird - 
           myCgb myCsb myRb myRs myS^2 mySird - 
           myCgs myCsb myRb myRs myS^2 mySird + myGds myRg mySirg + 
           myGm myRg mySirg + myCdb myRg myS mySirg + 
           myCgb myRg myS mySirg + myCgs myRg myS mySirg + 
           myCdb myGds myRb myRg myS mySirg + 
           myCgb myGds myRb myRg myS mySirg + 
           myCsb myGds myRb myRg myS mySirg + 
           myCdb myGm myRb myRg myS mySirg + 
           myCgb myGm myRb myRg myS mySirg + 
           myCsb myGm myRb myRg myS mySirg + 
           myCdb myGds myRg myRs myS mySirg + 
           myCgb myGds myRg myRs myS mySirg + 
           myCsb myGds myRg myRs myS mySirg + 
           myCdb myGm myRg myRs myS mySirg + 
           myCgb myGm myRg myRs myS mySirg + 
           myCsb myGm myRg myRs myS mySirg + 
           myCdb myCgs myRb myRg myS^2 mySirg + 
           myCgb myCgs myRb myRg myS^2 mySirg + 
           myCdb myCsb myRb myRg myS^2 mySirg + 
           myCgb myCsb myRb myRg myS^2 mySirg + 
           myCgs myCsb myRb myRg myS^2 mySirg + 
           myCdb myCgs myRg myRs myS^2 mySirg + 
           myCgb myCgs myRg myRs myS^2 mySirg + 
           myCdb myCsb myRg myRs myS^2 mySirg + 
           myCgb myCsb myRg myRs myS^2 mySirg + 
           myCgs myCsb myRg myRs myS^2 mySirg - myGds myRs mySirs - 
           myGm myRs mySirs - myCgs myRs myS mySirs - 
           myCdb myGds myRb myRs myS mySirs - 
           myCgb myGds myRb myRs myS mySirs - 
           myCsb myGds myRb myRs myS mySirs - 
           myCdb myGm myRb myRs myS mySirs - 
           myCgb myGm myRb myRs myS mySirs - 
           myCsb myGm myRb myRs myS mySirs - 
           myCdb myCgs myRb myRs myS^2 mySirs - 
           myCgb myCgs myRb myRs myS^2 mySirs - 
           myCdb myCsb myRb myRs myS^2 mySirs - 
           myCgb myCsb myRb myRs myS^2 mySirs - 
           myCgs myCsb myRb myRs myS^2 mySirs - myCdb myS Vb - 
           myCgb myS Vb - myCdb myGds myRs myS Vb - 
           myCgb myGds myRs myS Vb - myCsb myGds myRs myS Vb - 
           myCdb myGm myRs myS Vb - myCgb myGm myRs myS Vb - 
           myCsb myGm myRs myS Vb - myCdb myCgs myRs myS^2 Vb - 
           myCgb myCgs myRs myS^2 Vb - myCdb myCsb myRs myS^2 Vb - 
           myCgb myCsb myRs myS^2 Vb - myCgs myCsb myRs myS^2 Vb + 
           myGds Vg + myGm Vg + myCdb myS Vg + myCgb myS Vg + 
           myCgs myS Vg + myCdb myGds myRb myS Vg + 
           myCgb myGds myRb myS Vg + myCsb myGds myRb myS Vg + 
           myCdb myGm myRb myS Vg + myCgb myGm myRb myS Vg + 
           myCsb myGm myRb myS Vg + myCdb myGds myRs myS Vg + 
           myCgb myGds myRs myS Vg + myCsb myGds myRs myS Vg + 
           myCdb myGm myRs myS Vg + myCgb myGm myRs myS Vg + 
           myCsb myGm myRs myS Vg + myCdb myCgs myRb myS^2 Vg + 
           myCgb myCgs myRb myS^2 Vg + myCdb myCsb myRb myS^2 Vg + 
           myCgb myCsb myRb myS^2 Vg + myCgs myCsb myRb myS^2 Vg + 
           myCdb myCgs myRs myS^2 Vg + myCgb myCgs myRs myS^2 Vg + 
           myCdb myCsb myRs myS^2 Vg + myCgb myCsb myRs myS^2 Vg + 
           myCgs myCsb myRs myS^2 Vg - (myGds + 
              myGm + (myCgs + (myCdb + myCgb + myCsb) (myGds + 
                    myGm) myRb) myS + ((myCdb + 
                    myCgb) myCgs + (myCdb + myCgb + 
                    myCgs) myCsb) myRb myS^2) Vs - 
           myGmb (-Vb + 
              myRs (mySird + 
                 mySirs - (myCdb + myCgb) myS (myRg mySirg - Vb + 
                    Vg)) + Vs + 
              myRb ((-1 + (myCdb + myCgb) myRs myS) mySirb + (myCdb + 
                    myCgb) myS (-myRg mySirg + 
                    myRs (mySird + mySirs) - Vg + Vs))))) + 
     myCgb (-(1 + 
            myGds myRd + (myGds + myGm + myGmb) myRs) (myRb mySirb - 
           myRg mySirg + Vb - Vg) - 
        myCsb myS (-(1 + myGds myRd) myRs (myRg mySirg - Vb + Vg) + 
           myRb (myRs (mySid + mySirb + myGds myRd mySirb + mySirs + 
                 myGds (-myRg mySirg + myRd (mySird + mySirs) + Vd - 
                    Vg)) - (1 + myGds myRd) (myRg mySirg + Vg - 
                 Vs))) + 
        myCdb myS (myRd (1 + 
              myRs (myGds + myGm + myGmb + myCsb myS)) (myRg mySirg - 
              Vb + Vg) + 
           myRb ((1 + 
                 myRs (myGds + myGm + myGmb + 
                    myCsb myS)) (myRg mySirg - Vd + Vg) + 
              myRd (mySid - (1 + 
                    myRs (myGds + myGm + myGmb + myCsb myS)) mySirb - 
                 mySird - (myGds + myGm + myGmb + 
                    myCsb myS) (-myRg mySirg + 
                    myRs (mySird + mySirs) - Vg + 
                    Vs)))))))/(1 + (myGm + myGmb) myRs + (myCdb + 
      myCgb + myCsb) myRb myS + (myCdb myRd + myCgd myRd + 
      myCdb myGmb myRb myRd + myCgb myRg + myCgd myRg + myCgs myRg + 
      myCgd myGm myRd myRg + (myCgs + myCsb + myCsb myGm myRb + 
         myCgs myGmb myRg + (myGm + myGmb) (myCdb (myRb + myRd) + 
            myCgb (myRb + myRg) + 
            myCgd (myRd + myRg))) myRs) myS + (myCgd myCsb myRb myRd +
       myCgd myCsb myRb myRg + myCgs myCsb myRb myRg + 
      myCgd myCgs myRd myRg + 
      myCgd myCsb myGm myRb myRd myRg + (myCgs (myCsb (myRb + myRg) + 
            myCgd (myRd + myRg)) + 
         myCgd myCsb (myRg + myGm myRb myRg + 
            myRd (1 + myGm (myRb + myRg)))) myRs + 
      myCgb ((myCgs + myCsb) (myRg myRs + myRb (myRg + myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs))) + 
      myCdb (myCsb myRb myRd + myCgs myRb myRg + myCgs myRd myRg + 
         myCgs myGmb myRb myRd myRg + (myCsb (myRb + myRd) + 
            myCgs (myRb + myRd + myGmb myRb myRd + 
               myGmb (myRb + myRd) myRg)) myRs + 
         myCgb (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + 
                  myRg) myRs)))) myS^2 + (myCdb (myCgb + 
         myCgd) myCgs + myCgd myCgs myCsb + 
      myCdb (myCgb + myCgd + myCgs) myCsb + 
      myCgb myCgd (myCgs + myCsb)) (myRb myRd myRg + myRd myRg myRs + 
      myRb (myRd + myRg) myRs) myS^3 + 
   myGds (myRd + 
      myRs + ((myCdb + myCgb + myCsb) myRb myRd + (myCgb + myCgd + 
            myCgs) myRd myRg + (myCdb (myRb + myRd) + 
            myCsb (myRb + myRd) + 
            myCgb (myRb + myRg) + (myCgd + myCgs) (myRd + 
               myRg)) myRs) myS + (myCdb (myCgb + myCgd + 
            myCgs) + (myCgd + myCgs) myCsb + 
         myCgb (myCgd + myCgs + myCsb)) (myRb myRd myRg + 
         myRd myRg myRs + myRb (myRd + myRg) myRs) myS^2));
   Id = ((1 + myS ((myCgd + myCgs) myRg + myCgs myRs + 
         myCsb (myRb + myRs + (myCgd + myCgs) myRg myRs myS + 
            myRb ((myCgd + myCgs) myRg + myCgs myRs) myS) + 
         myCdb myRb (1 + ((myCgb + myCgd + myCgs) myRg + 
               myCgs myRs) myS) + 
         myCgb (myRb + 
            myRg + ((myCgd + myCgs + myCsb) myRb myRg + (myCgs + 
                  myCsb) (myRb + myRg) myRs) myS))) mySid - 
   myCdb myRb myS mySirb + myCgb myGm myRb myRg myS mySirb - 
   myCdb myGds myRb myRs myS mySirb - 
   myCsb myGds myRb myRs myS mySirb - 
   myCdb myGm myRb myRs myS mySirb - myCsb myGm myRb myRs myS mySirb -
    myCdb myCgb myRb myRg myS^2 mySirb - 
   myCdb myCgd myRb myRg myS^2 mySirb - 
   myCgb myCgd myRb myRg myS^2 mySirb - 
   myCdb myCgs myRb myRg myS^2 mySirb - 
   myCdb myCgs myRb myRs myS^2 mySirb - 
   myCdb myCsb myRb myRs myS^2 mySirb - 
   myCdb myCgb myGds myRb myRg myRs myS^2 mySirb - 
   myCdb myCgd myGds myRb myRg myRs myS^2 mySirb - 
   myCgb myCgd myGds myRb myRg myRs myS^2 mySirb - 
   myCdb myCgs myGds myRb myRg myRs myS^2 mySirb - 
   myCgb myCgs myGds myRb myRg myRs myS^2 mySirb - 
   myCgb myCsb myGds myRb myRg myRs myS^2 mySirb - 
   myCgd myCsb myGds myRb myRg myRs myS^2 mySirb - 
   myCgs myCsb myGds myRb myRg myRs myS^2 mySirb - 
   myCdb myCgb myGm myRb myRg myRs myS^2 mySirb - 
   myCdb myCgd myGm myRb myRg myRs myS^2 mySirb - 
   myCgb myCgd myGm myRb myRg myRs myS^2 mySirb - 
   myCgd myCsb myGm myRb myRg myRs myS^2 mySirb - 
   myCdb myCgb myCgs myRb myRg myRs myS^3 mySirb - 
   myCdb myCgd myCgs myRb myRg myRs myS^3 mySirb - 
   myCgb myCgd myCgs myRb myRg myRs myS^3 mySirb - 
   myCdb myCgb myCsb myRb myRg myRs myS^3 mySirb - 
   myCdb myCgd myCsb myRb myRg myRs myS^3 mySirb - 
   myCgb myCgd myCsb myRb myRg myRs myS^3 mySirb - 
   myCdb myCgs myCsb myRb myRg myRs myS^3 mySirb - 
   myCgd myCgs myCsb myRb myRg myRs myS^3 mySirb + myGds myRd mySird +
    myCdb myRd myS mySird + myCgd myRd myS mySird + 
   myCdb myGds myRb myRd myS mySird + 
   myCgb myGds myRb myRd myS mySird + 
   myCsb myGds myRb myRd myS mySird + 
   myCgb myGds myRd myRg myS mySird + 
   myCgd myGds myRd myRg myS mySird + 
   myCgs myGds myRd myRg myS mySird + 
   myCgd myGm myRd myRg myS mySird + 
   myCdb myGds myRd myRs myS mySird + 
   myCgd myGds myRd myRs myS mySird + 
   myCgs myGds myRd myRs myS mySird + 
   myCsb myGds myRd myRs myS mySird + 
   myCdb myGm myRd myRs myS mySird + myCgd myGm myRd myRs myS mySird +
    myCdb myCgb myRb myRd myS^2 mySird + 
   myCdb myCgd myRb myRd myS^2 mySird + 
   myCgb myCgd myRb myRd myS^2 mySird + 
   myCdb myCsb myRb myRd myS^2 mySird + 
   myCgd myCsb myRb myRd myS^2 mySird + 
   myCdb myCgb myRd myRg myS^2 mySird + 
   myCdb myCgd myRd myRg myS^2 mySird + 
   myCgb myCgd myRd myRg myS^2 mySird + 
   myCdb myCgs myRd myRg myS^2 mySird + 
   myCgd myCgs myRd myRg myS^2 mySird + 
   myCdb myCgb myGds myRb myRd myRg myS^2 mySird + 
   myCdb myCgd myGds myRb myRd myRg myS^2 mySird + 
   myCgb myCgd myGds myRb myRd myRg myS^2 mySird + 
   myCdb myCgs myGds myRb myRd myRg myS^2 mySird + 
   myCgb myCgs myGds myRb myRd myRg myS^2 mySird + 
   myCgb myCsb myGds myRb myRd myRg myS^2 mySird + 
   myCgd myCsb myGds myRb myRd myRg myS^2 mySird + 
   myCgs myCsb myGds myRb myRd myRg myS^2 mySird + 
   myCdb myCgb myGm myRb myRd myRg myS^2 mySird + 
   myCdb myCgd myGm myRb myRd myRg myS^2 mySird + 
   myCgb myCgd myGm myRb myRd myRg myS^2 mySird + 
   myCgd myCsb myGm myRb myRd myRg myS^2 mySird + 
   myCdb myCgs myRd myRs myS^2 mySird + 
   myCgd myCgs myRd myRs myS^2 mySird + 
   myCdb myCsb myRd myRs myS^2 mySird + 
   myCgd myCsb myRd myRs myS^2 mySird + 
   myCdb myCgb myGds myRb myRd myRs myS^2 mySird + 
   myCdb myCgd myGds myRb myRd myRs myS^2 mySird + 
   myCgb myCgd myGds myRb myRd myRs myS^2 mySird + 
   myCdb myCgs myGds myRb myRd myRs myS^2 mySird + 
   myCgb myCgs myGds myRb myRd myRs myS^2 mySird + 
   myCgb myCsb myGds myRb myRd myRs myS^2 mySird + 
   myCgd myCsb myGds myRb myRd myRs myS^2 mySird + 
   myCgs myCsb myGds myRb myRd myRs myS^2 mySird + 
   myCdb myCgb myGm myRb myRd myRs myS^2 mySird + 
   myCdb myCgd myGm myRb myRd myRs myS^2 mySird + 
   myCgb myCgd myGm myRb myRd myRs myS^2 mySird + 
   myCgd myCsb myGm myRb myRd myRs myS^2 mySird + 
   myCdb myCgb myGds myRd myRg myRs myS^2 mySird + 
   myCdb myCgd myGds myRd myRg myRs myS^2 mySird + 
   myCgb myCgd myGds myRd myRg myRs myS^2 mySird + 
   myCdb myCgs myGds myRd myRg myRs myS^2 mySird + 
   myCgb myCgs myGds myRd myRg myRs myS^2 mySird + 
   myCgb myCsb myGds myRd myRg myRs myS^2 mySird + 
   myCgd myCsb myGds myRd myRg myRs myS^2 mySird + 
   myCgs myCsb myGds myRd myRg myRs myS^2 mySird + 
   myCdb myCgb myGm myRd myRg myRs myS^2 mySird + 
   myCdb myCgd myGm myRd myRg myRs myS^2 mySird + 
   myCgb myCgd myGm myRd myRg myRs myS^2 mySird + 
   myCgd myCsb myGm myRd myRg myRs myS^2 mySird + 
   myCdb myCgb myCgs myRb myRd myRg myS^3 mySird + 
   myCdb myCgd myCgs myRb myRd myRg myS^3 mySird + 
   myCgb myCgd myCgs myRb myRd myRg myS^3 mySird + 
   myCdb myCgb myCsb myRb myRd myRg myS^3 mySird + 
   myCdb myCgd myCsb myRb myRd myRg myS^3 mySird + 
   myCgb myCgd myCsb myRb myRd myRg myS^3 mySird + 
   myCdb myCgs myCsb myRb myRd myRg myS^3 mySird + 
   myCgd myCgs myCsb myRb myRd myRg myS^3 mySird + 
   myCdb myCgb myCgs myRb myRd myRs myS^3 mySird + 
   myCdb myCgd myCgs myRb myRd myRs myS^3 mySird + 
   myCgb myCgd myCgs myRb myRd myRs myS^3 mySird + 
   myCdb myCgb myCsb myRb myRd myRs myS^3 mySird + 
   myCdb myCgd myCsb myRb myRd myRs myS^3 mySird + 
   myCgb myCgd myCsb myRb myRd myRs myS^3 mySird + 
   myCdb myCgs myCsb myRb myRd myRs myS^3 mySird + 
   myCgd myCgs myCsb myRb myRd myRs myS^3 mySird + 
   myCdb myCgb myCgs myRd myRg myRs myS^3 mySird + 
   myCdb myCgd myCgs myRd myRg myRs myS^3 mySird + 
   myCgb myCgd myCgs myRd myRg myRs myS^3 mySird + 
   myCdb myCgb myCsb myRd myRg myRs myS^3 mySird + 
   myCdb myCgd myCsb myRd myRg myRs myS^3 mySird + 
   myCgb myCgd myCsb myRd myRg myRs myS^3 mySird + 
   myCdb myCgs myCsb myRd myRg myRs myS^3 mySird + 
   myCgd myCgs myCsb myRd myRg myRs myS^3 mySird + myGm myRg mySirg - 
   myCgd myRg myS mySirg + myCdb myGm myRb myRg myS mySirg + 
   myCgb myGm myRb myRg myS mySirg + myCsb myGm myRb myRg myS mySirg -
    myCgd myGds myRg myRs myS mySirg - 
   myCgs myGds myRg myRs myS mySirg - 
   myCgd myGm myRg myRs myS mySirg + myCsb myGm myRg myRs myS mySirg -
    myCdb myCgb myRb myRg myS^2 mySirg - 
   myCdb myCgd myRb myRg myS^2 mySirg - 
   myCgb myCgd myRb myRg myS^2 mySirg - 
   myCgd myCsb myRb myRg myS^2 mySirg - 
   myCgd myCgs myRg myRs myS^2 mySirg - 
   myCgd myCsb myRg myRs myS^2 mySirg - 
   myCdb myCgb myGds myRb myRg myRs myS^2 mySirg - 
   myCdb myCgd myGds myRb myRg myRs myS^2 mySirg - 
   myCgb myCgd myGds myRb myRg myRs myS^2 mySirg - 
   myCdb myCgs myGds myRb myRg myRs myS^2 mySirg - 
   myCgb myCgs myGds myRb myRg myRs myS^2 mySirg - 
   myCgb myCsb myGds myRb myRg myRs myS^2 mySirg - 
   myCgd myCsb myGds myRb myRg myRs myS^2 mySirg - 
   myCgs myCsb myGds myRb myRg myRs myS^2 mySirg - 
   myCdb myCgb myGm myRb myRg myRs myS^2 mySirg - 
   myCdb myCgd myGm myRb myRg myRs myS^2 mySirg - 
   myCgb myCgd myGm myRb myRg myRs myS^2 mySirg - 
   myCgd myCsb myGm myRb myRg myRs myS^2 mySirg - 
   myCdb myCgb myCgs myRb myRg myRs myS^3 mySirg - 
   myCdb myCgd myCgs myRb myRg myRs myS^3 mySirg - 
   myCgb myCgd myCgs myRb myRg myRs myS^3 mySirg - 
   myCdb myCgb myCsb myRb myRg myRs myS^3 mySirg - 
   myCdb myCgd myCsb myRb myRg myRs myS^3 mySirg - 
   myCgb myCgd myCsb myRb myRg myRs myS^3 mySirg - 
   myCdb myCgs myCsb myRb myRg myRs myS^3 mySirg - 
   myCgd myCgs myCsb myRb myRg myRs myS^3 mySirg - myGds myRs mySirs -
    myGm myRs mySirs - myCdb myGds myRb myRs myS mySirs - 
   myCgb myGds myRb myRs myS mySirs - 
   myCsb myGds myRb myRs myS mySirs - 
   myCdb myGm myRb myRs myS mySirs - myCgb myGm myRb myRs myS mySirs -
    myCsb myGm myRb myRs myS mySirs - 
   myCgb myGds myRg myRs myS mySirs - 
   myCgd myGds myRg myRs myS mySirs - 
   myCgs myGds myRg myRs myS mySirs - 
   myCgb myGm myRg myRs myS mySirs - myCgd myGm myRg myRs myS mySirs -
    myCdb myCsb myRb myRs myS^2 mySirs - 
   myCgd myCgs myRg myRs myS^2 mySirs - 
   myCdb myCgb myGds myRb myRg myRs myS^2 mySirs - 
   myCdb myCgd myGds myRb myRg myRs myS^2 mySirs - 
   myCgb myCgd myGds myRb myRg myRs myS^2 mySirs - 
   myCdb myCgs myGds myRb myRg myRs myS^2 mySirs - 
   myCgb myCgs myGds myRb myRg myRs myS^2 mySirs - 
   myCgb myCsb myGds myRb myRg myRs myS^2 mySirs - 
   myCgd myCsb myGds myRb myRg myRs myS^2 mySirs - 
   myCgs myCsb myGds myRb myRg myRs myS^2 mySirs - 
   myCdb myCgb myGm myRb myRg myRs myS^2 mySirs - 
   myCdb myCgd myGm myRb myRg myRs myS^2 mySirs - 
   myCgb myCgd myGm myRb myRg myRs myS^2 mySirs - 
   myCgd myCsb myGm myRb myRg myRs myS^2 mySirs - 
   myCdb myCgb myCgs myRb myRg myRs myS^3 mySirs - 
   myCdb myCgd myCgs myRb myRg myRs myS^3 mySirs - 
   myCgb myCgd myCgs myRb myRg myRs myS^3 mySirs - 
   myCdb myCgb myCsb myRb myRg myRs myS^3 mySirs - 
   myCdb myCgd myCsb myRb myRg myRs myS^3 mySirs - 
   myCgb myCgd myCsb myRb myRg myRs myS^3 mySirs - 
   myCdb myCgs myCsb myRb myRg myRs myS^3 mySirs - 
   myCgd myCgs myCsb myRb myRg myRs myS^3 mySirs - myCdb myS Vb + 
   myCgb myGm myRg myS Vb - myCdb myGds myRs myS Vb - 
   myCsb myGds myRs myS Vb - myCdb myGm myRs myS Vb - 
   myCsb myGm myRs myS Vb - myCdb myCgb myRg myS^2 Vb - 
   myCdb myCgd myRg myS^2 Vb - myCgb myCgd myRg myS^2 Vb - 
   myCdb myCgs myRg myS^2 Vb - myCdb myCgs myRs myS^2 Vb - 
   myCdb myCsb myRs myS^2 Vb - myCdb myCgb myGds myRg myRs myS^2 Vb - 
   myCdb myCgd myGds myRg myRs myS^2 Vb - 
   myCgb myCgd myGds myRg myRs myS^2 Vb - 
   myCdb myCgs myGds myRg myRs myS^2 Vb - 
   myCgb myCgs myGds myRg myRs myS^2 Vb - 
   myCgb myCsb myGds myRg myRs myS^2 Vb - 
   myCgd myCsb myGds myRg myRs myS^2 Vb - 
   myCgs myCsb myGds myRg myRs myS^2 Vb - 
   myCdb myCgb myGm myRg myRs myS^2 Vb - 
   myCdb myCgd myGm myRg myRs myS^2 Vb - 
   myCgb myCgd myGm myRg myRs myS^2 Vb - 
   myCgd myCsb myGm myRg myRs myS^2 Vb - 
   myCdb myCgb myCgs myRg myRs myS^3 Vb - 
   myCdb myCgd myCgs myRg myRs myS^3 Vb - 
   myCgb myCgd myCgs myRg myRs myS^3 Vb - 
   myCdb myCgb myCsb myRg myRs myS^3 Vb - 
   myCdb myCgd myCsb myRg myRs myS^3 Vb - 
   myCgb myCgd myCsb myRg myRs myS^3 Vb - 
   myCdb myCgs myCsb myRg myRs myS^3 Vb - 
   myCgd myCgs myCsb myRg myRs myS^3 Vb + myGds Vd + myCdb myS Vd + 
   myCgd myS Vd + myCdb myGds myRb myS Vd + myCgb myGds myRb myS Vd + 
   myCsb myGds myRb myS Vd + myCgb myGds myRg myS Vd + 
   myCgd myGds myRg myS Vd + myCgs myGds myRg myS Vd + 
   myCgd myGm myRg myS Vd + myCdb myGds myRs myS Vd + 
   myCgd myGds myRs myS Vd + myCgs myGds myRs myS Vd + 
   myCsb myGds myRs myS Vd + myCdb myGm myRs myS Vd + 
   myCgd myGm myRs myS Vd + myCdb myCgb myRb myS^2 Vd + 
   myCdb myCgd myRb myS^2 Vd + myCgb myCgd myRb myS^2 Vd + 
   myCdb myCsb myRb myS^2 Vd + myCgd myCsb myRb myS^2 Vd + 
   myCdb myCgb myRg myS^2 Vd + myCdb myCgd myRg myS^2 Vd + 
   myCgb myCgd myRg myS^2 Vd + myCdb myCgs myRg myS^2 Vd + 
   myCgd myCgs myRg myS^2 Vd + myCdb myCgb myGds myRb myRg myS^2 Vd + 
   myCdb myCgd myGds myRb myRg myS^2 Vd + 
   myCgb myCgd myGds myRb myRg myS^2 Vd + 
   myCdb myCgs myGds myRb myRg myS^2 Vd + 
   myCgb myCgs myGds myRb myRg myS^2 Vd + 
   myCgb myCsb myGds myRb myRg myS^2 Vd + 
   myCgd myCsb myGds myRb myRg myS^2 Vd + 
   myCgs myCsb myGds myRb myRg myS^2 Vd + 
   myCdb myCgb myGm myRb myRg myS^2 Vd + 
   myCdb myCgd myGm myRb myRg myS^2 Vd + 
   myCgb myCgd myGm myRb myRg myS^2 Vd + 
   myCgd myCsb myGm myRb myRg myS^2 Vd + myCdb myCgs myRs myS^2 Vd + 
   myCgd myCgs myRs myS^2 Vd + myCdb myCsb myRs myS^2 Vd + 
   myCgd myCsb myRs myS^2 Vd + myCdb myCgb myGds myRb myRs myS^2 Vd + 
   myCdb myCgd myGds myRb myRs myS^2 Vd + 
   myCgb myCgd myGds myRb myRs myS^2 Vd + 
   myCdb myCgs myGds myRb myRs myS^2 Vd + 
   myCgb myCgs myGds myRb myRs myS^2 Vd + 
   myCgb myCsb myGds myRb myRs myS^2 Vd + 
   myCgd myCsb myGds myRb myRs myS^2 Vd + 
   myCgs myCsb myGds myRb myRs myS^2 Vd + 
   myCdb myCgb myGm myRb myRs myS^2 Vd + 
   myCdb myCgd myGm myRb myRs myS^2 Vd + 
   myCgb myCgd myGm myRb myRs myS^2 Vd + 
   myCgd myCsb myGm myRb myRs myS^2 Vd + 
   myCdb myCgb myGds myRg myRs myS^2 Vd + 
   myCdb myCgd myGds myRg myRs myS^2 Vd + 
   myCgb myCgd myGds myRg myRs myS^2 Vd + 
   myCdb myCgs myGds myRg myRs myS^2 Vd + 
   myCgb myCgs myGds myRg myRs myS^2 Vd + 
   myCgb myCsb myGds myRg myRs myS^2 Vd + 
   myCgd myCsb myGds myRg myRs myS^2 Vd + 
   myCgs myCsb myGds myRg myRs myS^2 Vd + 
   myCdb myCgb myGm myRg myRs myS^2 Vd + 
   myCdb myCgd myGm myRg myRs myS^2 Vd + 
   myCgb myCgd myGm myRg myRs myS^2 Vd + 
   myCgd myCsb myGm myRg myRs myS^2 Vd + 
   myCdb myCgb myCgs myRb myRg myS^3 Vd + 
   myCdb myCgd myCgs myRb myRg myS^3 Vd + 
   myCgb myCgd myCgs myRb myRg myS^3 Vd + 
   myCdb myCgb myCsb myRb myRg myS^3 Vd + 
   myCdb myCgd myCsb myRb myRg myS^3 Vd + 
   myCgb myCgd myCsb myRb myRg myS^3 Vd + 
   myCdb myCgs myCsb myRb myRg myS^3 Vd + 
   myCgd myCgs myCsb myRb myRg myS^3 Vd + 
   myCdb myCgb myCgs myRb myRs myS^3 Vd + 
   myCdb myCgd myCgs myRb myRs myS^3 Vd + 
   myCgb myCgd myCgs myRb myRs myS^3 Vd + 
   myCdb myCgb myCsb myRb myRs myS^3 Vd + 
   myCdb myCgd myCsb myRb myRs myS^3 Vd + 
   myCgb myCgd myCsb myRb myRs myS^3 Vd + 
   myCdb myCgs myCsb myRb myRs myS^3 Vd + 
   myCgd myCgs myCsb myRb myRs myS^3 Vd + 
   myCdb myCgb myCgs myRg myRs myS^3 Vd + 
   myCdb myCgd myCgs myRg myRs myS^3 Vd + 
   myCgb myCgd myCgs myRg myRs myS^3 Vd + 
   myCdb myCgb myCsb myRg myRs myS^3 Vd + 
   myCdb myCgd myCsb myRg myRs myS^3 Vd + 
   myCgb myCgd myCsb myRg myRs myS^3 Vd + 
   myCdb myCgs myCsb myRg myRs myS^3 Vd + 
   myCgd myCgs myCsb myRg myRs myS^3 Vd + myGm Vg - myCgd myS Vg + 
   myCdb myGm myRb myS Vg + myCgb myGm myRb myS Vg + 
   myCsb myGm myRb myS Vg - myCgd myGds myRs myS Vg - 
   myCgs myGds myRs myS Vg - myCgd myGm myRs myS Vg + 
   myCsb myGm myRs myS Vg - myCdb myCgb myRb myS^2 Vg - 
   myCdb myCgd myRb myS^2 Vg - myCgb myCgd myRb myS^2 Vg - 
   myCgd myCsb myRb myS^2 Vg - myCgd myCgs myRs myS^2 Vg - 
   myCgd myCsb myRs myS^2 Vg - myCdb myCgb myGds myRb myRs myS^2 Vg - 
   myCdb myCgd myGds myRb myRs myS^2 Vg - 
   myCgb myCgd myGds myRb myRs myS^2 Vg - 
   myCdb myCgs myGds myRb myRs myS^2 Vg - 
   myCgb myCgs myGds myRb myRs myS^2 Vg - 
   myCgb myCsb myGds myRb myRs myS^2 Vg - 
   myCgd myCsb myGds myRb myRs myS^2 Vg - 
   myCgs myCsb myGds myRb myRs myS^2 Vg - 
   myCdb myCgb myGm myRb myRs myS^2 Vg - 
   myCdb myCgd myGm myRb myRs myS^2 Vg - 
   myCgb myCgd myGm myRb myRs myS^2 Vg - 
   myCgd myCsb myGm myRb myRs myS^2 Vg - 
   myCdb myCgb myCgs myRb myRs myS^3 Vg - 
   myCdb myCgd myCgs myRb myRs myS^3 Vg - 
   myCgb myCgd myCgs myRb myRs myS^3 Vg - 
   myCdb myCgb myCsb myRb myRs myS^3 Vg - 
   myCdb myCgd myCsb myRb myRs myS^3 Vg - 
   myCgb myCgd myCsb myRb myRs myS^3 Vg - 
   myCdb myCgs myCsb myRb myRs myS^3 Vg - 
   myCgd myCgs myCsb myRb myRs myS^3 Vg - (myGds + 
      myGm + ((myCdb + myCgb + myCsb) (myGds + 
            myGm) myRb + ((myCgb + myCgd + myCgs) myGds + (myCgb + 
               myCgd) myGm) myRg) myS + (myCdb myCsb myRb + (((myCgb \
myCgs + myCdb (myCgb + myCgs) + (myCgb + myCgs) myCsb) myGds + 
               myCdb myCgb myGm) myRb + 
            myCgd (myCgs + (myCdb + myCgb + myCsb) (myGds + 
                  myGm) myRb)) myRg) myS^2 + (myCdb (myCgb + 
            myCgd) myCgs + myCgd myCgs myCsb + 
         myCdb (myCgb + myCgd + myCgs) myCsb + 
         myCgb myCgd (myCgs + myCsb)) myRb myRg myS^3) Vs + 
   myGmb (myCgd myRd myRs myS mySird + 
      myCgb myCgd myRd myRg myRs myS^2 mySird - 
      myCgd myRg myRs myS mySirg - myCgs myRg myRs myS mySirg - 
      myRs mySirs - myCgb myRg myRs myS mySirs - 
      myCgd myRg myRs myS mySirs - myCgs myRg myRs myS mySirs + Vb + 
      myCgb myRg myS Vb + myCgd myRg myS Vb + myCgs myRg myS Vb + 
      myCgs myRs myS Vb - myCgb myCgd myRg myRs myS^2 Vb + 
      myCgd myRs myS Vd + myCgb myCgd myRg myRs myS^2 Vd + 
      myCdb myRs myS (1 + (myCgb + myCgd + 
            myCgs) myRg myS) (myRd mySird - Vb + Vd) - 
      myCgd myRs myS Vg - 
      myCgs myRs myS Vg - (1 + (myCgb + myCgd + myCgs) myRg myS) Vs + 
      myRb ((1 + ((myCgb + myCgd + myCgs) myRg + (-myCdb + 
                  myCgs) myRs) myS - (myCgb myCgd + 
               myCdb (myCgb + myCgd + 
                  myCgs)) myRg myRs myS^2) mySirb + 
         myS (myCgb (myRg mySirg - myRs mySirs + Vg - Vs + 
               myCgd myS (myRd (myRg + myRs) mySird + myRs (Vd - Vg) -
                   myRg (myRs (mySirg + mySirs) - Vd + Vs))) + 
            myCdb (myRd (1 + (myCgb + myCgd + myCgs) (myRg + 
                    myRs) myS) mySird - myCgd myRg myRs myS mySirg - 
               myCgs myRg myRs myS mySirg - myRs mySirs - 
               myCgd myRg myRs myS mySirs - 
               myCgs myRg myRs myS mySirs + Vd + myCgd myRg myS Vd + 
               myCgs myRg myS Vd + myCgd myRs myS Vd + 
               myCgs myRs myS Vd - myCgd myRs myS Vg - 
               myCgs myRs myS Vg - (1 + (myCgd + myCgs) myRg myS) Vs -
                myCgb myS (myRs (-Vd + Vg) + 
                  myRg (myRs (mySirg + mySirs) - Vd + 
                    Vs)))))))/(1 + (myGm + myGmb) myRs + (myCdb + 
      myCgb + myCsb) myRb myS + (myCdb myRd + myCgd myRd + 
      myCdb myGmb myRb myRd + myCgb myRg + myCgd myRg + myCgs myRg + 
      myCgd myGm myRd myRg + (myCgs + myCsb + myCsb myGm myRb + 
         myCgs myGmb myRg + (myGm + myGmb) (myCdb (myRb + myRd) + 
            myCgb (myRb + myRg) + 
            myCgd (myRd + myRg))) myRs) myS + (myCgd myCsb myRb myRd +
       myCgd myCsb myRb myRg + myCgs myCsb myRb myRg + 
      myCgd myCgs myRd myRg + 
      myCgd myCsb myGm myRb myRd myRg + (myCgs (myCsb (myRb + myRg) + 
            myCgd (myRd + myRg)) + 
         myCgd myCsb (myRg + myGm myRb myRg + 
            myRd (1 + myGm (myRb + myRg)))) myRs + 
      myCgb ((myCgs + myCsb) (myRg myRs + myRb (myRg + myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs))) + 
      myCdb (myCsb myRb myRd + myCgs myRb myRg + myCgs myRd myRg + 
         myCgs myGmb myRb myRd myRg + (myCsb (myRb + myRd) + 
            
            myCgs (myRb + myRd + myGmb myRb myRd + 
               myGmb (myRb + myRd) myRg)) myRs + 
         myCgb (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + myRg) myRs)) + 
         myCgd (myRd myRg (1 + (myGm + myGmb) myRs) + 
            myRb (myRd + 
               myRg + (myGm + myGmb) myRd myRg + (myGm + 
                  myGmb) (myRd + 
                  myRg) myRs)))) myS^2 + (myCdb (myCgb + 
         myCgd) myCgs + myCgd myCgs myCsb + 
      myCdb (myCgb + myCgd + myCgs) myCsb + 
      myCgb myCgd (myCgs + myCsb)) (myRb myRd myRg + myRd myRg myRs + 
      myRb (myRd + myRg) myRs) myS^3 + 
   myGds (myRd + 
      myRs + ((myCdb + myCgb + myCsb) myRb myRd + (myCgb + myCgd + 
            myCgs) myRd myRg + (myCdb (myRb + myRd) + 
            myCsb (myRb + myRd) + 
            myCgb (myRb + myRg) + (myCgd + myCgs) (myRd + 
               myRg)) myRs) myS + (myCdb (myCgb + myCgd + 
            myCgs) + (myCgd + myCgs) myCsb + 
         myCgb (myCgd + myCgs + myCsb)) (myRb myRd myRg + 
         myRd myRg myRs + myRb (myRd + myRg) myRs) myS^2));
   Is = -Id - Ig - Ib;
   ToExpression[
    "{Icurg" <> name <> "==SCAPack`Private`Ig,Icurb" <> name <> 
     "==SCAPack`Private`Ib,Icurd" <> name <> "==SCAPack`Private`Id,Icurs" <> name <> 
     "==SCAPack`Private`Is}"]
   ];
MOSModelResistances["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Cgs, Global`Cgd, Global`Gm, Global`Gds, Global`Cds,
	Global`Cdb, Global`Gmb, Global`Csb, Global`Cgb, Global`Rg, Global`Rd, Global`Rs, Global`Rb, Global`IsNoisy}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

MOSModelBasic[Vd_, Vgp_, Vs_, Vb_, name_, Noisy_:False] := 
  Block[{Ig, Id, Ib, Is, myCgs = Global`Cgs, myCgd = Global`Cgd, 
    myS = Global`s, myGm = Global`Gm, myGds = Global`Gds, myCds = Global`Cds, 
    myCdb = Global`Cdb, myGmb = Global`Gmb, myCsb = Global`Csb, 
    myCgb = Global`Cgb, Inoise, Vg},
    If[ Noisy === True,
    	Vg = Vgp + ToExpression["Svg$"<>name];
    	Inoise = ToExpression["Sid$"<>name];,
    	Vg = Vgp;
    	Inoise = 0;
    ];
   Ig = -myCgb myS Vb - myCgd myS Vd + myCgb myS Vg + myCgd myS Vg + 
     myCgs myS Vg - myCgs myS Vs;
   Ib = myCdb myS Vb + myCgb myS Vb + myCsb myS Vb - myCdb myS Vd - 
     myCgb myS Vg - myCsb myS Vs;
   Id = myGmb Vb - myCdb myS Vb + myGds Vd + myCdb myS Vd + myCgd myS Vd + 
     myGm Vg - myCgd myS Vg - myGds Vs - myGm Vs - myGmb Vs + Inoise;
   Is = -Id - Ig - Ib;
   ToExpression[
    "{Icurg" <> name <> "==SCAPack`Private`Ig,Icurb" <> name <> 
     "==SCAPack`Private`Ib,Icurd" <> name <> "==SCAPack`Private`Id,Icurs" <> name <> 
     "==SCAPack`Private`Is}"]
   ];
MOSModelBasic["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Cgs, Global`Cgd, Global`Gm, Global`Gds, Global`Cds,
	Global`Cdb, Global`Gmb, Global`Csb, Global`Cgb, Global`IsNoisy}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

ResModel[Vp_, Vn_, name_, Rmodel_:False] := Block[{Irp, Irn, myGres = Global`Gvalue},
	If[ Rmodel === True,
  		myGres = 1/Global`Rvalue;
  	];
   Irp = (Vp - Vn)*myGres;
   Irn = -Irp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Irn,Icurp" <> name <> 
     "==SCAPack`Private`Irp}"]
   ];
ResModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Gvalue, Global`Rvalue},
	temp},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
	temp = line[[4]];
	temp = temp[[ ;;, 1 ]];
	If[ MemberQ[ temp, Global`Gvalue ] && MemberQ[ temp, Global`Rvalue ],
		Message[ ErrorCheckNetlist::doublepars, line[[3]] ];
		Abort[];
	];
];

ResNoisyModel[Vp_, Vn_, name_, Rmodel_:False] := 
  Block[{Irp, Irn, myGres = Global`Gvalue, Inoise},
  	If[ Rmodel === True,
  		myGres = 1/Global`Rvalue;
  	];
   Inoise = 
    Sqrt[4*Global`k*Global`T*myGres]*Noisy*ToExpression["Noisy$" <> name];
   Irp = (Vp - Vn)*myGres + Inoise;
   Irn = -Irp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Irn,Icurp" <> name <> 
     "==SCAPack`Private`Irp}"]
   ];
ResNoisyModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Gvalue, Global`Rvalue}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
	If[ MemberQ[ temp, Global`Gvalue ] && MemberQ[ temp, Global`Rvalue ],
		Message[ ErrorCheckNetlist::doublepars, line[[3]] ];
		Abort[];
	];
];

IndModel[Vp_, Vn_, name_] := 
  Block[{Ilp, Iln, myLind = Global`Lvalue, myS = Global`s},
   Ilp = (Vp - Vn)/(myLind*myS);
   Iln = -Ilp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Iln,Icurp" <> name <> 
     "==SCAPack`Private`Ilp}"]
   ];
IndModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Lvalue}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

CapModel[Vp_, Vn_, name_] := 
  Block[{Icp, Icn, myCcap = Global`Cvalue, myS = Global`s},
   Icp = myS*(Vp - Vn)*myCcap;
   Icn = -Icp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Icn,Icurp" <> name <> 
     "==SCAPack`Private`Icp}"]
   ];
CapModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`Cvalue}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

VoltModel[Vp_, Vn_, name_] := Block[{Ivp, Ivn},
   Ivp = ToExpression["Global`I$" <> name];
   Ivn = -Ivp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Ivn,Icurp" <> name <> 
     "==SCAPack`Private`Ivp}"]
   ];
VoltModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4]; 
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

CurModel[Vp_, Vn_, name_] := Block[{Icurp, Icurn},
   Icurp = -ToExpression["Global`I$" <> name];
   Icurn = -Icurp;
   ToExpression[
    "{Icurp" <> name <> "==SCAPack`Private`Icurp,Icurn" <> name <> 
     "==SCAPack`Private`Icurn}"]
   ];
CurModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

VoltNoiseModel[Vp_, Vn_, name_] := Block[{Ivnp, Ivnn},
   Ivnp = ToExpression["Global`I$" <> name]*Noisy*
     ToExpression["Noisy$" <> name];
   Ivnn = -Ivnp;
   ToExpression[
    "{Icurn" <> name <> "==SCAPack`Private`Ivnn,Icurp" <> name <> 
     "==SCAPack`Private`Ivnp}"]
   ];
VoltNoiseModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

CurNoiseModel[Vp_, Vn_, name_] := Block[{Icurnp, Icurnn, expr},
   Icurnp = -ToExpression["Global`I$" <> name]*Noisy*
     ToExpression["Noisy$" <> name];
   Icurnn = -Icurnp;
   Append[ToExpression[
	    "{Icurp" <> name <> "==SCAPack`Private`Icurnp,Icurn" <> name <> 
	     "==SCAPack`Private`Icurnn}"
     ],(Vp-Vn)(1-Noisy)==0]
   ];
CurNoiseModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

VoltProbeModel[Vp_, Vn_, name_] := Block[{Ivpp, Ivpn},
   Ivpp = 0;
   Ivpn = 0;
   ToExpression["{Icurp" <> name <> "==0,Icurn" <> name <> "==0}"]
   ];
VoltProbeModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

CurProbeModel[Vp_, Vn_, name_] := Block[{},
   Append[VoltModel[Vp, Vn, name], Vp - Vn == 0]
   ];
CurProbeModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
];

VCCSModel[Vpi_, Vni_, Vpo_, Vno_, name_] := 
  Block[{Icurno, Icurpo, myA = Global`A, Icurni, Icurpi},
   Icurpo = myA*(Vpi - Vni);
   Icurno = -Icurpo;
   Icurpi = 0;
   Icurni = 0;
   ToExpression[
    "{Icurpo" <> name <> "==SCAPack`Private`Icurpo,Icurno" <> name <> 
     "==SCAPack`Private`Icurno,Icurpi" <> name <> "==0,Icurni" <> name <> "==0}"]
   ];
VCCSModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`A}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

CCCSModel[Vpo_, Vno_, name_, srcname_] := 
  Block[{Icurno, Icurpo, myA = Global`A},
   Icurpo = ToExpression["Global`A*I$" <> srcname];
   Icurno = -Icurpo;
   ToExpression[
    "{Icurpo" <> name <> "==SCAPack`Private`Icurpo,Icurno" <> name <> 
     "==SCAPack`Private`Icurno}"]
   ];
CCCSModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`A, Global`probe}, probes, temp},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
	CheckRequiredParam[line[[4]], {Global`probe}];
	probes = Global`probe /. line[[4]];
	temp = Cases[ netlist, {"iprobe", _, _, _} ][[;;, 3]];
	If[ !MemberQ[ Flatten[ temp ], probes ],
		Message[ErrorCheckNetlist::prbnotfound, probes];
		Abort[];
	];
];

VCVSModel[Vpi_, Vni_, Vpo_, Vno_, name_] := 
 Block[{Icurni, Icurpi, Icurno, Icurpo, myA = Global`A, Vout},
  Icurpo = ToExpression["Global`I$" <> name];
  Icurno = -Icurpo;
  Icurni = 0;
  Icurpi = 0;
  Vout = If[Vpi === 0, "0", SymbolName[Vpi]];
  Vout = Vout <> If[Vni === 0, "-0", "-" <> SymbolName[Vni]];
  Vout = "Global`A*(" <> Vout <> ")==";
  Vout = Vout <> If[Vpo === 0, "0", SymbolName[Vpo]];
  Vout = Vout <> "-" <> If[Vno == 0, "0", SymbolName[Vno]];
  ToExpression[
   "{Icurno" <> name <> "==SCAPack`Private`Icurno,Icurpo" <> name <> 
    "==SCAPack`Private`Icurpo,Icurni" <> name <> "==0,Icurpi" <> name <> "==0," <>
     Vout <> "}"]
  ];
VCVSModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`A}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

CCVSModel[Vpo_, Vno_, name_, srcname_] := Block[{Icurno, Icurpo, Vout},
  Icurpo = ToExpression["Global`I$" <> name];
  Icurno = -Icurpo;
  Vout = If[Vpo === 0, "0", SymbolName[Vpo]];
  Vout = Vout <> If[Vno === 0, "-0", "-" <> SymbolName[Vno]];
  Vout = "Global`A*Global`I$" <> srcname <> "==(" <> Vout <> ")";
  ToExpression[
   "{Icurno" <> name <> "==SCAPack`Private`Icurno,Icurpo" <> name <> 
    "==SCAPack`Private`Icurpo," <> Vout <> "}"]
  ];
CCVSModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`A, Global`probe}, probes, temp},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 2];
	CheckRequiredParam[line[[4]], {Global`probe}];
	probes = Global`probe /. line[[4]];
	temp = Cases[ netlist, {"iprobe", _, _, _} ][[;;, 3]];
	If[ !MemberQ[ Flatten[ temp ], probes ],
		Message[ErrorCheckNetlist::prbnotfound, probes];
		Abort[];
	];
];

NullorModel[Vpi_, Vni_, Vpo_, Vno_, name_] := 
  Block[{Icurno, Icurpo, Icurni, Icurpi, Vieqn},
   Icurni = 0;
   Icurpi = 0;
   Icurpo = ToExpression["I$" <> name];
   Icurno = -Icurpo;
   Vieqn = If[Vpi === 0, "0", SymbolName[Vpi]];
   Vieqn = Vieqn <> "-" <> If[Vni === 0, "0", SymbolName[Vni]];
   Vieqn = Vieqn <> "==0";
   ToExpression[
    "{Icurno" <> name <> "==SCAPack`Private`Icurno,Icurpo" <> name <> 
     "==SCAPack`Private`Icurpo,Icurni" <> name <> "==0,Icurpi" <> name <> "==0," <>
      Vieqn <> "}"]
   ];
NullorModel["ErrorCheck"][line_, netlist_] := Block[{validParams={}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

GyratorModel[Vpi_, Vni_, Vpo_, Vno_, name_] := 
 Block[{Icurno, Icurpo, Icurni, Icurpi, Vi, Vo},
  Vi = Vpi - Vni;
  Vo = Vpo - Vno;
  Icurpi = Vo/Global`A;
  Icurni = -Icurpi;
  Icurpo = Vi/Global`A;
  Icurno = -Icurpo;
  ToExpression[
   "{Icurpi" <> name <> "==SCAPack`Private`Icurpi,Icurni" <> name <> 
    "==SCAPack`Private`Icurni,Icurpo" <> name <> "==SCAPack`Private`Icurpo,Icurno" <> 
    name <> "==SCAPack`Private`Icurno}"]
  ];
GyratorModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`A}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

TransformerModel[Vpi_, Vni_, Vpo_, Vno_, name_] := 
  Block[{Icurns, Icurps, Icurnp, Icurpp, Vi, Vo, Veqn, Ieqn},
   Vi = Vpi - Vni;
   Vo = Vpo - Vno;
   Icurpp = ToExpression["I$p$" <> name];
   Icurps = -ToExpression["I$s$" <> name];
   Icurnp = -Icurpp;
   Icurns = -Icurps;
   Veqn = Vi*Global`TurnsS == Vo*Global`TurnsP;
   Ieqn = Icurpp*Global`TurnsP == -Icurps*Global`TurnsS;
   ToExpression[
    "{SCAPack`Private`Veqn,SCAPack`Private`Ieqn,Icurpp" <> name <> 
     "==SCAPack`Private`Icurpp,Icurps" <> name <> "==SCAPack`Private`Icurps,Icurnp" <>
      name <> "==SCAPack`Private`Icurnp,Icurns" <> name <> "==SCAPack`Private`Icurns}"]
   ];
TransformerModel["ErrorCheck"][line_, netlist_] := Block[{validParams={Global`TurnsS, Global`TurnsP}},
	CheckElementCount[line, 4];
	CheckParameterList[line[[4]], validParams];
	CheckNodeCount[line, 4];
];

SubcircuitModel[nodes_, name_, netlist_] := Block[{temp, netNodes, blockNodes},
   (* identify all the nodes in the netlist that need to be rewritten *)
   
   netNodes = Flatten[Tally[Flatten[netlist[[;; , 2]]]][[;; , 1]]];
   blockNodes = Append[nodes, 0];
   temp = # -> Symbol[(name <> "$" <> SymbolName[#])] & /@ 
     Complement[netNodes, blockNodes];
   
   (* we're just going to return a corrected hierarchy to the caller *)
   
   netlist /. temp
   ];
SubcircuitModel["ErrorCheck"][line_, netlist_] := Block[{subckt},
	CheckElementCount[line, 4];
	subckt = line[[4]];
	subckt = Cases[subckt, {x_/;StringMatchQ[x,"subckt"], _, _, _}];
	If[ Length[subckt] > 0,
		Message[ErrorCheckNetlist::nestedsubckt];
		Abort[]
	];
];

NodesetModel[nodes_, name_, noderulelist_] := Block[{temp},
	(* we're going to convert the rules into equations which are then 
	   converted back to rules and eliminated by FormEquations[] *)
	#[[1]] == #[[2]] & /@ noderulelist
];
NodesetModel["ErrorCheck"][line_, netlist_] := Block[{params, temp, nodes},
	CheckElementCount[line, 4];
	params = line[[4]];
	
	(* check for rules *)
	If[ Length[ params ] == 0, Message[ErrorCheckNetlist::reqparam, "nodeset rules"]; Abort[]; ];
	
	(* check for rules only in the param *)
	temp = DeleteCases[ params, _Rule ];
	If[ Length[ temp ] > 0,
		Message[ErrorCheckNetlist::invalidparam, params];
		Abort[];
	];
	
	(* check for nodes existing in the netlist *)
	temp = params[[ ;;, 1 ]];
	nodes = DeleteDuplicates[ DeleteCases[ Flatten[ netlist[[ ;;, 2 ]] ], 0 ] ];
	temp = Cases[ {#, MemberQ[ nodes, # ]} & /@ temp, {_, False} ];
	If[ Length[ temp ] > 0,
		Message[ ErrorCheckNetlist::nodenotfound, # ] & /@ temp[[ ;;, 1 ]]; 
		Abort[];
	];
];

(* ::Section:: *)
(* Netlisting and equation solving mechanics *)

ModelDispatcher[ itemName_ ] := Block[{matchFunc=StringMatchQ[ itemName, #, IgnoreCase -> True ] &},
	Piecewise[{
        	{MOSModelBasic, matchFunc["mos"]},
        	{MOSModelResistances, matchFunc["mosr"]},
        	{ResModel, matchFunc["res"]},
        	{IndModel, matchFunc["ind"]},
        	{CapModel, matchFunc["cap"]},
        	{VoltModel, matchFunc["volt"]},
        	{VoltProbeModel, matchFunc["vprobe"]},
        	{CurModel, matchFunc["cur"]},
        	{CurProbeModel, matchFunc["iprobe"]},
        	{VoltNoiseModel, matchFunc["voltnoise"]},
        	{CurNoiseModel, matchFunc["curnoise"]},
        	{ResNoisyModel, matchFunc["resnoisy"]},
        	{VCCSModel, matchFunc["vccs"]},
        	{VCVSModel, matchFunc["vcvs"]},
        	{CCVSModel, matchFunc["ccvs"]},
        	{CCCSModel, matchFunc["cccs"]},
        	{NullorModel, matchFunc["nullor"]},
        	{GyratorModel, matchFunc["gyrator"]},
        	{TransformerModel, matchFunc["xfmr"]},
        	{NodesetModel, matchFunc["nodeset"]}},
        	Message[NetlistCircuit::unknownelem, itemName];Abort[]
        ]
];

CheckRequiredParam[ list_, needed_ ] := Block[{temp, i},
	If[ Length[needed] > 0,
		temp = Cases[list, (x_->_)->x]; (* list of provided parameters *)
		If[ Length[temp] == 0,
			Table[ Message[ErrorCheckNetlist::reqparam, i], {i, needed} ];
			Abort[];
		]; (* a quick out for obviously missing parameters *)
		temp = {#, MemberQ[ temp, # ]} & /@ needed;
		If[ Not[Or@@temp[[;;,2]]],
			Table[ Message[ErrorCheckNetlist::reqparam, i], {i, Cases[ temp, {_, False} ] }];
			Abort[];
		];
	]
];

CheckNodeCount[ line_, number_ ] := Block[{},
	If[Length[line[[2]]] != number, Message[ErrorCheckNetlist::wrongnodes, line]; Abort[]]
]

CheckElementCount[ line_, numelems_ ] := Block[{},
	If[Length[line] != numelems, Message[ErrorCheckNetlist::invalidline, line]; Abort[]]
];

GetParameterList[ list_ ] := Block[{},
	Cases[ list, (x_ -> _) -> x ]
];

CheckParameterList[ list_, valid_ ] := Block[{temp, listP},
	listP = GetParameterList[ list ];
	temp = MemberQ[ valid, # ] & /@ listP; 
	If[ Not[#[[1]]], Message[ ErrorCheckNetlist::invalidparam, #[[2]] ] ] & /@ Partition[ Riffle[temp, listP], 2 ];
	If[ Not[Or@@temp] && ( Length[valid] > 0 ) && ( Length[temp] > 0 ), Abort[] ];
];

AddSubcircuits[netlist_] := Block[{subckts, altNetlist},
	altNetlist = LowerCaseNetlist[netlist];
   subckts = Cases[altNetlist, {q_String, _, _, _}/;(ToLowerCase[q]=="subckt")];
   If[Length[subckts] > 0,
    subckts = SubcircuitModel[#[[2]], #[[3]], First[#[[4]]]] & /@ subckts;
    altNetlist = DeleteCases[altNetlist, {"subckt", _, _, _}];
    altNetlist = Join[subckts[[1]], altNetlist];
    altNetlist,
    netlist
    ]
   ];

CreateNodeEquations[sharedNodeInfo_] := 
  Block[{entries, i, device, node, name, curExpr},
   entries = Length[sharedNodeInfo];
   curExpr = Reap[For[i = 1, i <= entries, i++,
       device = sharedNodeInfo[[i, 1]];
       node = sharedNodeInfo[[i, 2]];
       name = sharedNodeInfo[[i, 3]];
       Sow["Icur" <> node <> name];
       ]][[2]];
   ToExpression[StringJoin[Riffle[Flatten[curExpr], "+"]] <> "==0"]
   ];

NetlistCircuit[netlist_, intSkipCheck_: False] := 
  Block[{uniqNodes, elements, i, curType, curNodes, curName, eqns, curModel, 
    curEqns, x, curTerm, termList, devEqns, nodeEqns, curChangeList, 
    includeGndCurrent = False, altNetlist, subckts, srcName},
   (* we need to fold in our subcircuits before we do any processing *)
   
   altNetlist = AddSubcircuits[netlist];
   
   If[! intSkipCheck, ErrorCheckNetlist[altNetlist]];
   uniqNodes = DeleteCases[DeleteDuplicates[Flatten[altNetlist[[;; , 2]]]], 0];
   elements = Length[altNetlist];
   eqns = Reap[For[i = 1, i <= elements, i++,
        curName = altNetlist[[i, 3]];
        curType = ToLowerCase[altNetlist[[i, 1]]];
        curChangeList = altNetlist[[i, 4]];
        curNodes = altNetlist[[i, 2]];
        curModel = ModelDispatcher[ curType ];
        Switch[curType,
         "cccs" | "ccvs",
         srcName = Global`probe /. curChangeList;
         curEqns = 
          Apply[curModel, Flatten[{curNodes, curName, srcName}]] //.curChangeList;,
         "nodeset",
         curEqns = curModel[curNodes, curName, curChangeList];,
         "res" | "resnoisy",
         If[ MemberQ[ curChangeList[[ ;;, 1]], Global`Gvalue ],
	         curEqns = Apply[curModel, Flatten[{curNodes, curName, False}]] //.curChangeList;,
           	 curEqns = Apply[curModel, Flatten[{curNodes, curName, True}]] //.curChangeList;
         ];,
         "mos" | "mosr",
         If[ MemberQ[ curChangeList[[ ;;, 1]], Global`IsNoisy ],
	         curEqns = Apply[curModel, Flatten[{curNodes, curName, Global`IsNoisy /. curChangeList}]] //.curChangeList;,
           	 curEqns = Apply[curModel, Flatten[{curNodes, curName, False}]] //.curChangeList;
         ];,
         _,
         curEqns = 
           Apply[curModel, Flatten[{curNodes, curName}]] //. curChangeList;
         ];
        Sow[curEqns, "eqns"];
        termList = Piecewise[{
        	{{1->"d",2->"g",3->"s",4->"b"},(curType=="mos")||(curType=="mosr")},
        	{{1->"p",2->"n"},(curType=="res")||(curType=="resnoisy")||(curType=="ind")||(curType=="cap")||(curType=="volt")||(curType=="iprobe")||(curType=="vprobe")||(curType=="cur")||(curType=="curnoise")||(curType=="voltnoise")},
        	{{1->"pi",2->"ni",3->"po",4->"no"},(curType=="vccs")||(curType=="vcvs")},
        	{{1->"po",2->"no"},(curType=="cccs")||(curType=="ccvs")},
        	{{1->"pi",2->"ni",3->"po",4->"no"},(curType=="nullor")||(curType=="gyrator")},
        	{{1->"pp",2->"np",3->"ps",4->"ns"},curType=="xfmr"},
        	{{1->"p", 2->"n"},curType=="nodeset"}},
        	Message[NetlistCircuit::termerror]; Abort[]
        ];
        
        For[x = 1, x <= Length[curNodes], x++,
         curTerm = x /. termList;
         If[includeGndCurrent || ! NumericQ[curNodes[[x]]], 
          Sow[{curModel, curTerm, curName}, curNodes[[x]]]]
         ];
        ];][[2]];
   devEqns = eqns[[1]];
   nodeEqns = Map[CreateNodeEquations, eqns[[2 ;;]]];
   eqns = Flatten[{nodeEqns, devEqns}];
   FixedPoint[FormEquations[#, GetSolveVariables[altNetlist, True]] &, eqns]
   ];

FormEquations[eqnList_, solveVars_] := Block[{eqns, temp, x, y, solveEqs},
   eqns = Flatten[eqnList];
   
   (* The following eliminates var==
   0 expressions only if var is not a solve variable, 
   otherwise it performs the simplification and then keeps the solve variable \
eqn *)
   temp = Cases[eqns, x_Symbol == _];
   eqns = DeleteCases[eqns, x_Symbol == _];
   solveEqs = Cases[temp, x_Symbol == _ /; MemberQ[solveVars, x]];
   temp = temp //. (x_Symbol == y_) -> (x -> y);
   eqns = eqns //. temp;
   eqns = Flatten[{eqns, solveEqs}];
   
   (* The following eliminates 0==
   var expressions only if var is not a solve variable, 
   otherwise it performs the simplification and then keeps the solve variable \
eqn *)
   temp = Cases[eqns, _ == x_Symbol];
   eqns = DeleteCases[eqns, _ == x_Symbol];
   solveEqs = Cases[temp, _ == x_Symbol /; MemberQ[solveVars, x]];
   temp = temp //. (y_ == x_Symbol) -> (x -> y);
   eqns = eqns //. temp;
   eqns = Flatten[{eqns, solveEqs}];
   
   (* The following eliminates any tautologies in the eqn set *)
   
   eqns = DeleteCases[eqns, True];
   
   eqns
   ];

GetSolveVariables[netlist_, skipAddSubCkt_: False] := 
 Block[{vsrc, isrc, vprb, iprb, nullors, nodes, ans, findFunc, 
   blockNodes = {}, altNetlist, vcvss, ccvss, xfmrs, nodesets, mosns},
  (* add all subcircuit elements into the netlist first *)
  
  altNetlist = If[skipAddSubCkt, netlist, AddSubcircuits[netlist]];
  altNetlist = LowerCaseNetlist[ altNetlist ];
  findFunc = Cases[altNetlist, {#, _, _, _}] &;
  
  (* Find all voltage sources *)
  vsrc = findFunc["volt"];
  ans = ToExpression["I$" <> #[[3]]] & /@ vsrc;
  blockNodes = 
   Flatten[Append[blockNodes, Last[DeleteCases[#[[2]], 0]]] & /@ vsrc];(* 
  arbitrarily only solve for the positive terminal *)
  
  (* Find all voltage probes *)
  vprb = findFunc["vprobe"];
  ans = Join[ans, #[[2]] & /@ vprb];
  ans = Flatten[ans];
  
  (* Find all current probes *)
  iprb = findFunc["iprobe"];
  ans = Join[ans, ToExpression["I$" <> #[[3]]] & /@ iprb];
  blockNodes = 
   Flatten[Append[blockNodes, 
     If[! NumberQ[#[[2, 1]]], #[[2, 1]], #[[2, 2]]] & /@ iprb]];
  
  (* Find all current sources *)
  isrc = findFunc["cur"];
  ans = Join[ans, ToExpression["I$" <> #[[3]]] & /@ isrc];
  
  (* Find all nullors *)
  nullors = findFunc["nullor"];
  ans = Join[ans, ToExpression["I$" <> #[[3]]] & /@ nullors];
  
  (* Find all vcvs *)
  vcvss = findFunc["vcvs"];
  ans = Join[ans, ToExpression["I$" <> #[[3]]] & /@ vcvss ];
  
  (* Find all ccvs *)
  ccvss = findFunc["ccvs"];
  ans = Join[ans, ToExpression["I$" <> #[[3]]] & /@ ccvss ];
  
  (* Find all xfmrs *)
  xfmrs = findFunc["xfmr"];
  ans = Join[ans, ToExpression["I$s$" <> #[[3]]] & /@ xfmrs ];
  ans = Join[ans, ToExpression["I$p$" <> #[[3]]] & /@ xfmrs ];
  
  (* Find all nodesets *)
  nodesets = findFunc["nodeset"];
  ans = Join[ans, Flatten[ Function[x, x[[1]]] /@ #[[4]] & /@ nodesets ] ];
  
  (* Find all voltage nodes that are not connected to a voltage source, 
  one end of a current probe, or a voltage probe *)
  
  nodes = Flatten[#[[2]] & /@ 
     DeleteCases[altNetlist, {"volt", _, _, _} | {"vprobe", _, _, _}]];
  ans = Join[ans, Complement[nodes, ans, blockNodes]];
  
  (* Delete all references to ground *)
  ans = DeleteCases[Flatten[ans], 0]
  ];

ErrorCheckNetlist[netlist_] := Block[{temp, i, temp2},
	
   (* Check for a valid netlist format *)
   temp = Cases[{netlist}, {_List..}];
   If[Length[temp] == 0, 
    Message[ErrorCheckNetlist::incompletenetlist]; Abort[];
    ];
   
   (* Find duplicate element names *)
   temp = #[[3]] & /@ netlist;
   If[Length[temp] != Length[DeleteDuplicates[temp]], 
    Message[ErrorCheckNetlist::dupenames, 
     Cases[Tally[temp], {_, q_ /; q > 1}][[;; , 1]]]; Abort[];];
   
   (* Now find nodes with a single connection *)
   
   temp = Cases[Tally[Flatten[#[[2]] & /@ netlist]], {_, q_ /; q == 1}];
   If[Length[temp] > 0,
    Message[ErrorCheckNetlist::oneconn, temp[[;; , 1]]];
    Abort[]
    ];
   
   (* Now find two terminal elements whose nodes are all identical *)
   
   temp = Cases[netlist, {_, {x_, x_}, _, _}];
   If[Length[temp] > 0,
    Message[ErrorCheckNetlist::shortedelement, #[[3]]] & /@ temp;
    Abort[]
    ];
   
   (* Check for a valid symbol name for the nodes *)
   
   temp = DeleteDuplicates[Flatten[netlist[[;; , 2]]]];
   temp = DeleteCases[temp, _Symbol | 0];
   If[Length[temp] > 0,
    Message[ErrorCheckNetlist::invalidnodename, #] & /@ temp;
    Abort[]
    ];
    
    (* have each netlist item passed through the model's error checker *)
    
    temp = ModelDispatcher /@ netlist[[;;, 1]];
    temp2 = LowerCaseNetlist[ netlist ];
    Table[temp[[i]]["ErrorCheck"][temp2[[i]],temp2],{i,1,Length[netlist]}];
   ];

LowerCaseNetlist[netlist_] := Block[{},
   Join[{ToLowerCase[#[[1]]]}, Rest[#]] & /@ netlist
   ];
   
SolveFullSystem[netlist_, moreEqns_:{}] := 
  Block[{eqns, vars, ans, findFunc = Cases[netlist, {#, _, _, _}] &, noiseElim, temp},
  	noiseElim = {Noisy -> 0};
  	temp = Cases[ netlist, {"mos", _, _, {___, Global`IsNoisy -> True, ___} } ]; 
	If[ Length[temp] > 0,
	  	noiseElim = Append[ noiseElim, ToExpression["Sid$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Svg$" <> #[[3]]] -> 0 ] & /@ temp;
	];
  	temp = Cases[ netlist, {"mosr", _, _, {___, Global`IsNoisy -> True, ___} } ];
  	If[ Length[temp] > 0,
	  	noiseElim = Append[ noiseElim, ToExpression["Sid$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Svg$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Sirg$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Sird$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Sirs$" <> #[[3]]] -> 0 ] & /@ temp;
	  	noiseElim = Append[ noiseElim, ToExpression["Sirb$" <> #[[3]]] -> 0 ] & /@ temp;
  	];
  	noiseElim = Flatten[ noiseElim ];
   eqns = Join[ NetlistCircuit[netlist] /. noiseElim, moreEqns ];
   vars = GetSolveVariables[netlist];
   ans = Solve[eqns, vars]
   ];

CheckIndependentSources[ netlist_ ] := Block[{temp, altered},
	altered = LowerCaseNetlist[ netlist ];
	temp = Cases[ altered, {"volt", _, _, _} ];
	temp = Join[ temp, Cases[ altered, {"cur", _, _, _} ] ];
	temp = Join[ temp, Cases[ altered, {"voltnoise", _, _, _} ] ];
	temp = Join[ temp, Cases[ altered, {"curnoise", _, _, _} ] ];
	Length[ temp ]
];

(* ::Section:: *)
(* Automated Analyses *)

GetTransferFunction[netlist_ , {output_, input_}] := Block[{ans},
   ans = SolveFullSystem[netlist];
   Simplify[output/input /. ans]
   ];
   
DisableIndependentSources[netlist_List, saveList_List] := 
  Block[{altered, temp, extraEqns, temp2, cinetlist, i},
   
   (* change netlist to lower case *)
   cinetlist = LowerCaseNetlist[netlist];
    
   (* open circuit all current sources *)
   temp = Cases[cinetlist, {"cur", _, _, _} | {"curnoise", _, _, _}];
   temp = DeleteCases[temp, {_, _, x_ /; ! MemberQ[saveList,x], _}];
   altered = Join[DeleteCases[cinetlist, {"cur", _, _, _} | {"curnoise", _, _, _}], temp];
   altered = DeleteCases[altered, {}];
     
   (* short circuit all voltage sources *)  
   temp = Cases[cinetlist, {"volt", _, _, _}|{"voltnoise", _, _, _}];
   temp2 = Flatten[ Cases[temp, {_, _, #, _}] & /@ saveList, 1 ];
   temp = Flatten[DeleteCases[temp, {_, _, x_ /; StringMatchQ[x, #, IgnoreCase -> True], _}] & /@ saveList, 1];
   extraEqns = (#[[2, 1]] == #[[2, 2]]) & /@ temp;
   altered = Join[DeleteCases[altered, {"volt", _, _, _} | {"voltnoise", _, _, _}], temp2];
   
   (* now eliminate any of the voltage sources nodes that are NOT also nodes for the saveList elements *)
   temp = Table[i[[2, 1]] -> i[[2, 2]], {i, temp}];
   temp2 = 
    DeleteCases[
     Flatten[Cases[cinetlist, {"volt"|"voltnoise"|"cur"|"curnoise", _, x_ /; MemberQ[saveList, x], _}][[;; , 2]]],
      0];
   (* temp = DeleteCases[temp, x_ /; MemberQ[temp2, x] -> _]; *)
   altered = altered /. temp;
   
   (* also prune off any extra equations for nodes where we have nodes in our \
saveList sources *)
   extraEqns = 
    DeleteCases[
     extraEqns, ((x_ /; MemberQ[temp2, x]) == (_)) | ((_) == (x_ /; 
          MemberQ[temp2, x]))];
   
   {altered, extraEqns, temp}
   ];

GetNoisePSD[netlist_List, {output_, ref_}, srcname_String] := 
  Block[{altered, curNodes, temp, extraEqns, eqns, nSrc, vars, curnSrc, 
    voltnSrc, tf, solveAns, znoise, resnSrc, cinetlist, mosnSrc, mosrnSrc},
   (* error checking *)
   ErrorCheckNetlist[netlist];
   
   cinetlist = LowerCaseNetlist[netlist];
   altered = cinetlist;
   
   temp = Cases[
     altered, {"curnoise", _, srcname, _} | {"voltnoise", _, 
       srcname, _} | {"resnoisy", _, srcname, _} | {"mos", _, srcname, {___, Global`IsNoisy -> True, ___}} |
       {"mosr", _, srcname, {___, Global`IsNoisy -> True, ___}}
       ];
   If[Length[temp] != 1, Message[GetNoisePSD::srcnotfound, srcname]; 
    Abort[]]; 
   
   (* disable independent sources *)
   temp = DisableIndependentSources[netlist, {srcname}];
   altered = temp[[1]];
   extraEqns = temp[[2]];
   
   (* now find single dangling nodes resulting from our manglings *)
   temp = Cases[Tally[Flatten[#[[2]] & /@ altered]], {_, q_ /; q == 1}][[;; , 
      1]];
   (altered = DeleteCases[altered, {_, {#, _}, _, _} | {_, {_, #}, _, _}]) & /@
     temp;
   
   (* now find elements that have been shorted out and delete them *)
   altered = DeleteCases[altered, {_, {x_, x_}, _, _}];
   
   (* identify all the noise sources and subtract out our src of interest *)
   curnSrc = Cases[cinetlist, {"curnoise", _, _, _}][[;; , 3]];
   voltnSrc = Cases[cinetlist, {"voltnoise", _, _, _}][[;; , 3]];
   resnSrc = Cases[cinetlist, {"resnoisy", _, _, _}][[;; , 3]];
   mosnSrc = Cases[cinetlist, {"mos", _, _, {___, Global`IsNoisy -> True, ___}}][[;;, 3]];
   mosrnSrc = Cases[cinetlist, {"mosr", _, _, {___, Global`IsNoisy -> True, ___}}][[;;, 3]];
   nSrc = Join[curnSrc, voltnSrc, resnSrc, mosnSrc, mosrnSrc];
   nSrc = DeleteCases[nSrc, srcname];
   
   (* now form the full system of equations, enabling noise analysis *)
   eqns = Flatten[Append[NetlistCircuit[altered] /. Noisy -> 1, extraEqns]];
   eqns = eqns /. Table[ToExpression["Noisy$" <> i] -> 0, {i, nSrc}];
   eqns = eqns /. ToExpression["Noisy$" <> srcname] -> 1;
   
   (* create everything for solving the system *)
   vars = GetSolveVariables[altered];
   temp = Cases[ cinetlist, {"mos", _, x_, {___, Global`IsNoisy -> True, ___}} -> x ];
   temp = Join[ temp, Cases[ cinetlist, {"mosr", _, x_, {___, Global`IsNoisy -> True, ___}} -> x ] ]; 
   temp = Complement[ temp, {srcname} ];
   eqns = FormEquations[eqns, vars] /. Table[ToExpression["Svg$"<>i]->0, {i, temp}]/. Table[ToExpression["Sid$"<>i]->0, {i, temp}];
   
   (* solve the system *)
   solveAns = Solve[eqns, vars];
   
   (* compute the transfer function and noise impedance *)
   temp = Cases[altered, {_, _, srcname, _}][[1]]; 
   If[MemberQ[curnSrc, srcname],
    (* this is a current noise source, input is a current *)
    
    tf = ((output - ref)/ToExpression["I$" <> srcname])^2;
    znoise = ((temp[[2, 1]] - temp[[2, 2]])/ToExpression["I$" <> srcname]);
    ];
   If[MemberQ[voltnSrc, srcname],
    (* this is a voltage noise source, 
    input is the voltage across the source *)
    
    tf = ((output - ref)/(temp[[2, 1]] - temp[[2, 2]]))^2;
    znoise = ((temp[[2, 1]] - temp[[2, 2]])/ToExpression["I$" <> srcname]);
    ];
   If[MemberQ[resnSrc, srcname],
    (* this is a resistance, bring its contribution to the output *)
    
    tf = (output - ref)^2;
    znoise = Null;
    ];
    If[MemberQ[mosnSrc, srcname] || MemberQ[mosrnSrc, srcname],
    	(* this is a mos, bring its flicker and thermal noise to the output *)
    	tf = (output - ref)^2;
    	znoise = Null;
    ];
   
   (* return our answer to the caller *)
   Flatten[{ToExpression["tf$" <> srcname] -> tf, 
      ToExpression["zn$" <> srcname] -> znoise} /. solveAns]
   ];

GetTotalNoisePSD[netlist_List, output_List] := Block[{nSrc, ciNetlist},
	ciNetlist = LowerCaseNetlist[ netlist ];
   	nSrc = Cases[ciNetlist, {"curnoise", _, _, _} | {"voltnoise", _, _, _} | 
   		{"resnoisy", _, _, _} | {"mos", _, _, {___, Global`IsNoisy -> True, ___}}][[;; ,
       3]];
   	Flatten[GetNoisePSD[netlist, output, #] & /@ nSrc]
   ];

GetInputImpedance[netlist_, nodes_, intSkipCheck_: False] := 
  Block[{altered, currentName, extraEqns, temp, vars, eqns, solveAns, simplifications},
   (* error checking *)
   If[! intSkipCheck,
    ErrorCheckNetlist[netlist];
    temp = MemberQ[Flatten[netlist[[;; , 2]]], #] & /@ nodes;
    If[! (And @@ temp),
     Message[GetInputImpedance::nodenotfound, #] & /@ Pick[nodes, Not /@ temp];
     Abort[];
     ];
    ];
   
   (* ensure that there are no independent sources left in the netlist *)
   temp = Cases[ altered, {"volt", _, _, _} | {"cur", _, _, _} ];
   If[ Length[ temp ] > 0,
   	Message[ GetInputImpedance::indsrcfnd ];
   	Abort[];
   ];
   
   (* add a current source *)
   currentName = "I" <> ToString[Unique[]];
   altered = Join[netlist, {{"Cur", nodes, currentName, {}}}];
   
   (* disable all sources *)
   
   temp = DisableIndependentSources[altered, {currentName}];
   altered = temp[[1]];
   extraEqns = temp[[2]];
   simplifications = temp[[3]];
   
   (* prune elements with dangling nodes *)
   
   temp = Flatten[altered[[;; , 2]]];
   temp = Flatten[Cases[Tally[temp], {_, q_ /; q == 1}][[;; , 1]]];
   (altered = DeleteCases[altered, {_, x_ /; MemberQ[x, #], _, _}]) & /@ 
    temp;
   
   (* check for infinite impedance *)
   
   temp = Tally[Flatten[altered[[;; , 2]]]];
   
   (* form system *)
   vars = GetSolveVariables[altered];
   eqns = NetlistCircuit[altered, True];
   eqns = Join[eqns, extraEqns];
   eqns = FormEquations[eqns, vars];
   
   (* solve system *)
   solveAns = Quiet[Solve[eqns, vars]];
   temp = -(ToExpression["I$" <> currentName]/(nodes[[2]] - nodes[[1]]))^-1;
   Quiet[(temp /. solveAns /. simplifications)]
   ];

GetZVTimeConstants[netlist_] := 
  Block[{temp, inds, caps, aElements, altered, nodes, inpImp, inpImpInd, capValues, 
  	timeConstants, tf, indNodes, indValues, probeNodes},
   (* error checking *)
   ErrorCheckNetlist[netlist];
   altered = LowerCaseNetlist[netlist];
   
   (* ensure that there are no independent sources left in the netlist *)
   temp = Cases[ altered, {"volt", _, _, _} | {"cur", _, _, _} ];
   If[ Length[ temp ] > 0,
   	Message[ GetZVTimeConstants::indsrcfnd ];
   	Abort[];
   ];
   
   (* find each inductor and short circuit with a iprobe *)
   inds = Cases[ altered, {"ind", _, _, _}];
   
   (* find each capacitor and open circuit *)
   caps = Cases[
     altered, {"cap", _, _, _} | {"mos", _, _, _} | {"mosr", _, _, _}];
   altered = DeleteCases[altered, {"cap", _, _, _}];
   
   (* for each mos line, set their caps to zero *)
   
   temp = Position[altered, {"mos", _, _, _} | {"mosr", _, _, _}];
   altered = 
    altered /. {(Global`Cgs -> _) -> Null, (Global`Cds -> _) -> 
       Null, (Global`Cgd -> _) -> Null, (Global`Cdb -> _) -> Null, 
       (Global`Csb -> _) -> Null, (Global`Cgb -> _) -> Null};
   (altered[[#, 4]] = 
       Flatten[Join[
         altered[[#, 4]], {Global`Cgs -> 0, Global`Cds -> 0, 
          Global`Cgd -> 0, Global`Cdb -> 0, Global`Cgb -> 0, Global`Csb -> 0}]]) & /@ temp;
   
   (* determine all cap nodes for input impedance analysis *)

   nodes = Flatten[
     Block[{capLine = #, model = #[[1]], ans = {}, capNodes = #[[2]]},
        Switch[model,
         "cap",
         ans = Append[ans, capNodes];,
         ("mos") | ("mosr"),
         ans = Append[ans, {capNodes[[1]], capNodes[[2]]}]; (* Cdg *)
          ans = Append[ans, {capNodes[[2]], capNodes[[3]]}]; (* Cgs *)
           ans = Append[ans, {capNodes[[1]], capNodes[[3]]}]; (* Cds *)
           ans = Append[ans, {capNodes[[1]], capNodes[[4]]}]; (* Cdb *)
           ans = Append[ans, {capNodes[[2]], capNodes[[4]]}]; (* Cgb *)
           ans = Append[ans, {capNodes[[3]], capNodes[[4]]}]; (* Csb *)
         ];
        ans
        ] & /@ caps, 1];
   
   indNodes = inds[[;;, 2]];
   
   (* add voltage probes to all nodes of interest *)
   probeNodes = DeleteCases[ DeleteDuplicates[ Flatten[ Join[ nodes, indNodes ] ] ], 0 ];
   (altered = Append[ altered, {"vprobe", {#, 0}, SymbolName[Unique["vp"]], {}} ]) & /@ probeNodes;
   
   (* determine the input impedance for all the ind nodes we've found *)
   inpImpInd = Reap[
   	Block[{tempNetlist, thisInd = #[[3]], thisPort = #[[2]], allInds = inds[[;;, 3]], i},
   		tempNetlist = DeleteCases[ altered, {"ind", _, thisInd, _} ]; (* OC our inductor *)
   		tempNetlist = tempNetlist /. { { "ind", x_, y_, _ } -> { "iprobe", x, y, {} } }; (* SC all others *)
   		Sow[ GetInputImpedance[ tempNetlist, thisPort, True ] ];
   	] & /@ inds;
   ];
   inpImpInd = Flatten[ inpImpInd[[2]], 2 ];
   
   (* determine the input impedance for all the cap nodes we've found *)
   altered = altered /. { {"ind", x_, y_, _List} -> {"iprobe", x, y, {}} };
   inpImp = Flatten[GetInputImpedance[altered, #, True] & /@ nodes];
   
   (* determine the capacitor for each of the nodes *)
   
   capValues = 
    Flatten[Block[{capLine = #, capParams = #[[4]], capModel = #[[1]], 
         ans = {}},
        Switch[capModel,
         "cap",
         ans = Append[ans, Global`Cvalue /. capParams];,
         "mos" | "mosr",
         ans = Append[ans, Global`Cgd /. capParams];
         ans = Append[ans, Global`Cgs /. capParams];
         ans = Append[ans, Global`Cds /. capParams];
         ans = Append[ans, Global`Cdb /. capParams];
         ans = Append[ans, Global`Cgb /. capParams];
         ans = Append[ans, Global`Csb /. capParams]; 
         ];
        ans
        ] & /@ caps];
        
    (* determine the inductor for each of the nodes *)
    indValues = Global`Lvalue /. inds[[;;, 4]];
   
   (* figure out the time constants *)
   timeConstants = Join[ capValues*inpImp, indValues/inpImpInd ]
   ];
   
DisableSources[ netlist_List, srcChanges_List ] := Block[ {altered, temp, opens, shorts, nodes, newProbes},
	(* check for errors in the netlist *)
	altered = LowerCaseNetlist[ netlist ]; 
	ErrorCheckNetlist[ altered ];
	
	If[ Length[ srcChanges ] == 0,
		Message[ DisableSources::nochglist ];
		Abort[];
	];
	
	(* check for validity for the source change list *)
	temp = DeleteCases[ srcChanges, ( x_String -> "Open" ) | ( y_String -> "Short" )  ];
	If[ Length[ temp ] > 0,
		Message[ DisableSources::invalidchglist, #] & /@ temp;
		Abort[]; 
	];
	
	(* get the shorts and opens setup for each *)
	opens = Flatten[ Cases[ srcChanges, ( x_String -> "Open" ) -> x ] ];
	shorts = Flatten[ Cases[ srcChanges, ( x_String -> "Short" ) -> x ] ];
	
	(* check that the sources being requested are valid *)
	temp = Cases[ altered, ( {"cur", _, x_, _} | {"volt", _, x_, _} ) -> x ];
	temp = MemberQ[temp, # ] & /@ Join[ shorts, opens ];
	If[ Not[ And@@temp ],
		Message[ DisableSources::invalidsrc, # ] & /@ 
			Cases[ Partition[ Riffle[ temp, Join[ shorts, opens ] ], 2 ], { False, x_ } -> x ];
		Abort[];
	];
	
	(* get nodes that are being deleted *)
	nodes = Flatten[ (Cases[ altered, {_, _, #, _} ] & /@ opens), 1 ][[ ;;, 2 ]];
	nodes = DeleteCases[ Flatten[ nodes ], 0 ];
	newProbes = { "vprobe", { #, 0 }, SymbolName[ Unique["vp"] ], {} } & /@ nodes;
	altered = Join[ newProbes, altered ];
	
	(* open circuit all elements desired *)
	If[ Length[ opens ] > 0,
		altered = DeleteCases[ altered, { _, _, #, _ } ] & /@ opens;
	]; 

	(* short circuit all elements desired *)
	temp = { _, x_, y_/;MemberQ[ shorts, y ], {} } -> { "iprobe", x, y, {} }; 
	altered = altered /. temp;
	
	(* final circuit massage *)
	If[ Length[ netlist ] == Length[ altered ],
		altered,
		altered[[1]] 
	]
]; 

CheckValidPorts[ netlist_, ports_ ] := Block[ {nodes, temp, portNodes},
	nodes = DeleteDuplicates[ Flatten[ netlist[[;;, 2]] ] ];
	portNodes = DeleteDuplicates[ Flatten[ ports ] ];
	temp = MemberQ[ nodes, # ]& /@ portNodes;
	If[ Not[And@@temp],
		Message[ ComputeNetwork::portnotfound ];
		Return[ $Failed ];
	];
];

ComputeNetwork["Z"][ netlist_, ports_ ] := Block[{altered, temp, numPorts, stimPort, stimPortName},
	(* basic error checking *)
	altered = LowerCaseNetlist[ netlist ]; 
	ErrorCheckNetlist[ altered ];
	temp = CheckIndependentSources[ altered ];
	If[ temp > 0,
		Message[ ComputeNetwork::indsrcfnd ];
		Return[ $Failed ];
	];
	CheckValidPorts[ netlist, ports ];
	
	(* for each port, we begin by
		1. add current source to port i
		2. measure all voltages for port x
		3. repeat for all ports
	*)
	
	Transpose[ Reap[ Table[
		stimPort = i;
		stimPortName = SymbolName[Unique["I"]]; 
		temp = Join[ {{"cur", stimPort, stimPortName, {}}}, altered ];
		temp = Quiet[ SolveFullSystem[ temp ] ];
		temp = (Apply[Subtract, #] & /@ ports )/ToExpression["I$"<>stimPortName]/.temp;
		Sow[ Flatten[temp] ]; 
		, {i, ports}	
	] ][[ 2, 1 ]] ]
];

ComputeNetwork["Y"][ netlist_, ports_ ] := Block[{altered, temp, numPorts, stimPort, stimPortName, portMap, otherPorts, expr},
	(* basic error checking *)
	altered = LowerCaseNetlist[ netlist ];
	ErrorCheckNetlist[ altered ];
	temp = CheckIndependentSources[ altered ];
	If[ temp > 0,
		Message[ ComputeNetwork::indsrcfnd ];
		Abort[];
	];
	CheckValidPorts[ netlist, ports ];
	
	(* for each port, we begin by
		1. add voltage source to port i
		2. measure all currents for port x
		3. repeat for all ports
	*)
	
	Reap[ Table[
		stimPort = i;
		stimPortName = SymbolName[Unique["I"]]; 
		temp = Join[ {{"volt", stimPort, stimPortName, {}}}, altered ];
		otherPorts = Complement[ ports, {stimPort} ];
		portMap = Table[ otherPorts[[x]] -> SymbolName[Unique["I"]], {x, 1, Length[otherPorts]} ];
		Table[ temp = Join[ {{"iprobe", x[[1]], x[[2]], {}}}, temp ], {x, portMap} ];  
		temp = Quiet[ SolveFullSystem[ temp ] ];
		expr = Insert[ Flatten[ ("-I$" <> #) & /@ portMap[[;;,2]] ], "-I$" <> stimPortName, Position[ ports, stimPort ] ];
		temp = ToExpression[ expr ]/Subtract@@stimPort /. temp;
		
		Sow[ Flatten[temp] ]; 
		, {i, ports}	
	] ][[ 2, 1 ]]
];

ComputeNetwork["H"][ netlist_, ports_ ] := Block[{altered, temp, numPorts, stimPort, stimPortName, portMap, ans},
	(* basic error checking *)
	altered = LowerCaseNetlist[ netlist ];
	ErrorCheckNetlist[ altered ];
	temp = CheckIndependentSources[ altered ];
	If[ temp > 0,
		Message[ ComputeNetwork::indsrcfnd ];
		Abort[];
	];
	If[ Length[ ports ] != 2,
		Message[ ComputeNetwork::twoports ];
		Abort[];
	];
	CheckValidPorts[ netlist, ports ];
	
	(* for each port, we begin by
		1. add voltage source to port i
		2. measure all currents for port x
		3. repeat for all ports
	*)
	
	(* let's get port 1 setup *)
	stimPort = SymbolName[ Unique["I"] ];
	portMap = SymbolName[ Unique["I"] ];
	temp = Join[ { {"cur", ports[[1]], stimPort, {} } }, altered];
	temp = Join[ { {"iprobe", ports[[2]], portMap, {} } }, temp ]; 
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	ans = {{Subtract@@ports[[1]], -ToExpression["I$" <> portMap]}/ToExpression["I$"<>stimPort]};
	ans = ans /. temp;
	(* now on to port 2 *)
	stimPort = SymbolName[ Unique["V"] ];
	temp = Join[ {{"volt", ports[[2]], stimPort, {} }}, altered ];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	ans = Append[ ans, {Subtract@@ports[[1]], -ToExpression["I$" <> stimPort ]}/(Subtract@@ports[[2]])/.temp ];
	Transpose[ ans ]
];

ComputeNetwork["G"][ netlist_, ports_ ] := Block[{altered, temp, numPorts, stimPort, stimPortName, portMap, ans},
	(* basic error checking *)
	altered = LowerCaseNetlist[ netlist ];
	ErrorCheckNetlist[ altered ];
	temp = CheckIndependentSources[ altered ];
	If[ temp > 0,
		Message[ ComputeNetwork::indsrcfnd ];
		Abort[];
	];
	If[ Length[ ports ] != 2,
		Message[ ComputeNetwork::twoports ];
		Abort[];
	];
	CheckValidPorts[ netlist, ports ];
	
	(* for each port, we begin by
		1. add voltage source to port i
		2. measure all currents for port x
		3. repeat for all ports
	*)
	
	(* let's get port 1 setup *)
	stimPort = SymbolName[ Unique["V"] ];
	temp = Join[ { { "volt", ports[[1]], stimPort, {} } }, altered ];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	ans = { {-ToExpression["I$" <> stimPort], Subtract@@ports[[2]]}/(Subtract@@ports[[1]]) }/. temp;
	(* now on to port 2 *)
	stimPort = SymbolName[ Unique["I"] ];
	portMap = SymbolName[ Unique["I"] ];
	temp = Join[ { {"cur", ports[[2]], stimPort, {} } }, altered];
	temp = Join[ { {"iprobe", ports[[1]], portMap, {} } }, temp ]; 
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	ans = Append[ ans, {-ToExpression["I$" <> portMap ], Subtract@@ports[[2]]}/ToExpression["I$"<>stimPort]/.temp ];
	Transpose[ ans ]
];

ComputeNetwork["ABCD"][ netlist_, ports_ ] := Block[{altered, temp, numPorts, stimPort, stimPortName, portMap, A, B, C, D},
	(* basic error checking *)
	altered = LowerCaseNetlist[ netlist ];
	ErrorCheckNetlist[ altered ];
	temp = CheckIndependentSources[ altered ];
	If[ temp > 0,
		Message[ ComputeNetwork::indsrcfnd ];
		Abort[];
	];
	If[ Length[ ports ] != 2,
		Message[ ComputeNetwork::twoports ];
		Abort[];
	];
	CheckValidPorts[ netlist, ports ];
	
	Off[ Power::infy, Infinity::indet ];
	
	(* A *)
	stimPort = SymbolName[Unique["V"]];
	portMap = SymbolName[Unique["V"]];
	temp = Join[ {{"volt", ports[[1]], stimPort, {}}}, altered ];
	temp = Join[ {{"vprobe", ports[[2]], portMap, {}}}, temp ];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]]; 
	A = ( Subtract@@ports[[2]]/Subtract@@ports[[1]] ) /. temp;
	
	(* D *)
	stimPort = SymbolName[Unique["I"]];
	portMap = SymbolName[Unique["I"]];
	temp = Join[ {{"cur", ports[[1]], stimPort, {}}}, altered ];
	temp = Join[ {{"iprobe", ports[[2]], portMap, {}}}, temp ];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]]; 
	D = ToExpression["I$" <> portMap]/ToExpression["I$" <> stimPort];
	D = D /. temp;
	
	(* C *)
	stimPort = SymbolName[Unique["V"]];
	portMap = SymbolName[Unique["I"]];
	temp = Join[ {{"volt", ports[[1]], stimPort, {}}}, altered ];
	temp = Join[ {{"iprobe", ports[[2]], portMap, {}}}, temp ];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	C = ToExpression["I$" <> portMap]/Subtract@@ports[[1]]; 
	C = C /. FilterRules[ temp, ToExpression["I$" <> portMap] ]; 
	C = C /. temp;
	C = C /. { ToExpression["I$" <> stimPort] -> 0 }; 
	
	(* B *)
	stimPort = SymbolName[Unique["I"]];
	portMap = SymbolName[Unique["V"]];
	temp = Join[ {{"cur", ports[[1]], stimPort, {}}}, altered ];
	temp = Join[ {{"vprobe", ports[[2]], portMap, {}}}, temp];
	temp = Quiet[ SolveFullSystem[ temp ] ][[1]];
	B = Subtract@@ports[[2]]/ToExpression["I$" <> stimPort];
	B = B /. temp;
	
	On[ Power::infy, Infinity::indet ];
	
	{{A, B}, {C, D}} /. {Indeterminate -> 0, ComplexInfinity -> 0}
];

ComputeZNetwork[ netlist_, ports_ ] := Block[{altered, temp},
	NetworkData[ { "Z" -> ComputeNetwork["Z"][ netlist, ports ] } ]
];

ComputeYNetwork[ netlist_, ports_ ] := Block[{},
	NetworkData[ { "Y" -> ComputeNetwork["Y"][ netlist, ports ] } ]
];

ComputeHNetwork[ netlist_, ports_ ] := Block[{},
	NetworkData[ { "H" -> ComputeNetwork["H"][ netlist, ports ] } ]
];

ComputeGNetwork[ netlist_, ports_ ] := Block[{},
	NetworkData[ { "G" -> ComputeNetwork["G"][ netlist, ports ] } ]
];

ComputeABCDNetwork[ netlist_, ports_ ] := Block[{},
	NetworkData[ { "ABCD" -> ComputeNetwork["ABCD"][ netlist, ports ] } ]
];

Normal[ NetworkData[ parList_ ] ] ^:= Block[{},
	parList[[ ;;, 2 ]][[1]]
];
Format[ NetworkData[ parList_ ] ] ^:= Block[{temp, z0data},
	temp = StringJoin@@Riffle[(# <> "-parameter" & /@ DeleteCases[parList[[;; , 1]], "Z0"]), ", "];
	z0data = "Z0" /. parList;
	If[ Not[ ( (z0data == {}) || (z0data == "Z0") ) ],
		temp = temp <> ", Z0=" <> ToString[z0data];
	];
	"NetworkData["<>ToString[ temp ]<>", <rawdata>]"
];

NetworkData[ rawdata_, type_ ] := Block[{ upperType = ToUpperCase[type], temp, test },
	If[ Not[ MemberQ[{"ABCD", "Y", "Z", "G", "H" }, upperType ] ],
		Message[NetworkData::unknfmt, type];
		Return[$Failed]; 
	];
	If[ Head[ rawdata =!= List ],
		Message[NetworkData::baddata];
		Return[$Failed];
	];
	Switch[ upperType,
		"Z" || "Y",
			temp = Dimensions[ rawdata ];
			test = True;
			test = test && ( Length[ temp ] == 2 );
			test = test && ( temp[[1]] == temp[[2]] );
			If[ Not[test],
				Message[NetworkData::baddata];
				Return[$Failed];
			];,
		"G" || "H" || "ABCD",
			temp = Dimensions[ rawdata ];
			test = temp[[1]] == temp[[2]];
			test = test && ( Length[ temp ] == 2 );
			test = test && ( temp[[1]] == 2 );
			If[ Not[test],
				Message[NetworkData::baddata];
				Return[$Failed];
			];
	];
	NetworkData[ { type -> rawdata } ]
]

SymmetricQ[ data_ ] := Block[{netType, raw},
	If[ Head[ data ] =!= NetworkData,
		Message[NetworkProperties::netwdata];
		Return[$Failed];
	];
	netType = data[[ 1, 1, 1 ]];
	raw = data[[ 1, 1, 2 ]];
	SymmetricQ[raw, netType]
];
SymmetricQ[ data_, "Z" ] := Block[{},
	data[[1, 1]] == data[[2, 2]]
];
SymmetricQ[ data_, "Y" ] := Block[{},
	data[[1, 1]] == data[[2, 2]]
];
SymmetricQ[ data_, "G" ] := Block[{newPars},
	$Failed
];
SymmetricQ[ data_, "H" ] := Block[{newPars},
	$Failed
];
SymmetricQ[ data_, "ABCD" ] := Block[{},
	data[[1, 1]] == data[[2, 2]]
];

LosslessQ[ data_ ] := Block[{netType, raw},
	If[ Head[ data ] =!= NetworkData,
		Message[NetworkProperties::netwdata];
		Return[$Failed];
	];
	netType = data[[ 1, 1, 1 ]];
	raw = data[[ 1, 1, 2 ]];
	LosslessQ[raw, netType]
];
LosslessQ[ data_, "Z" ] := Block[{index,len, i, j, temp},
	len = Length[ data ];
	index = Table[{i,i},{i,1,len}];
	temp = LowerTriangularize[data, -1] + UpperTriangularize[data, 1];
	And@@(ComplexExpand[Re[ # ] == 0] & /@ Flatten[ temp ])//Simplify
];
LosslessQ[ data_, "Y" ] := Block[{index, len, i, j, temp},
	len = Length[ data ];
	index = Table[{i,i},{i,1,len}]; 
	temp = LowerTriangularize[data, -1] + UpperTriangularize[data, 1];
	And@@(ComplexExpand[Re[ # ] == 0] & /@ Flatten[ temp ])//Simplify
];
LosslessQ[ data_, "G" ] := Block[{newPars},
	$Failed
];
LosslessQ[ data_, "H" ] := Block[{newPars},
	$Failed
];
LosslessQ[ data_, "ABCD" ] := Block[{},
	$Failed
];

ReciprocalQ[ data_, "Z" ] := Block[{temp1, temp2},
	temp1 = Transpose[ UpperTriangularize[ data, 1 ] ]//Flatten;
	temp2 = LowerTriangularize[ data, 1 ]//Flatten;
	And@@((#[[1]] == #[[2]])& /@ Partition[ Riffle[temp1, temp2], 2 ] // Simplify) // Simplify
];
ReciprocalQ[ data_, "Y" ] := Block[{temp1, temp2},
	temp1 = Transpose[ UpperTriangularize[ data, 1 ] ]//Flatten;
	temp2 = LowerTriangularize[ data, 1 ]//Flatten;
	And@@((#[[1]] == #[[2]])& /@ Partition[ Riffle[temp1, temp2], 2 ] // Simplify) // Simplify
];
ReciprocalQ[ data_, "G" ] := Block[{newPars},
	$Failed
];
ReciprocalQ[ data_, "H" ] := Block[{newPars},
	$Failed
];
ReciprocalQ[ data_, "ABCD" ] := Block[{},
	Det[ data ] == 1
];
ReciprocalQ[ data_ ] := Block[{netType, raw},
	If[ Head[ data ] =!= NetworkData,
		Message[NetworkProperties::netwdata];
		Return[$Failed];
	];
	netType = data[[ 1, 1, 1 ]];
	raw = data[[ 1, 1, 2 ]];
	ReciprocalQ[raw, netType]
];

End[]

EndPackage[]
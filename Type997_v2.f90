Subroutine Type997
!-----------------------------------------------------------------------------------------------------------------------
! This subroutine models a horizontal ground heat exchanger system.
!
! Modifications: 
!    July   28th,    2008 : Coding of New Capabilities into Previous 997 model
!    August  5th,    2008 : Fixed a bug in the vertical noding for multiple soil layers
!    August  6th,    2008 : Fixed a bug where the Have_Pipe array was being set based on another unit's parameters
!    August 12th,    2008 : Fixed a bug in the calculation of the k_above and k_below values
!    August 15th,    2008 : Added surface energy balance terms as outputs, fixed a bug when re-reading the soil parameters
!    September 2nd,  2008 : Fixed another bug in the calculation of the k_below values
!    September 12th, 2008 : Reset the unit number in the after-convergence call to make sure the parameters are reread if there is more than one unit.
!    September 15th, 2008 : Fixed a bug in the output writing section.
!    November 5th,   2008 : Fixed a typo in the calculation of the deep earth temperature
!    November 11th,  2008 : Fixed the storage of the side boundary temperatures as they were not getting updated
!    December 1st,   2008 : Fixed a problem where the section for the number of calculated inlets for repeat reads was deleted
!    September 3rd,  2009 : Fixed a small problem when checking the depth of the first HX layer
!    December 11th,  2009 : Fixed the calculation of the conduction from the near-field surface in surface modes 2 and 3
!    December 11th,  2009 : Fixed the calculation of the conduction from the zone surface in surface mode 3
!    January 19th,   2010 : Fixed a small problem when checking for convergence of the far-field temperatures
!    May             2012 : TPM - conversion to version 17 coding standards
!    November 12th   2012 : JWT - updated the output file to be consistent with the soil temperature viewer and increased the max iterations
!-----------------------------------------------------------------------------------------------------------------------
! Copyright © 2012 Thermal Energy System Specialists, LLC. All rights reserved.

!Export this subroutine for its use in external DLLs.
!DEC$ATTRIBUTES DLLEXPORT :: TYPE997

!Use Statements
Use TrnsysConstants
Use TrnsysFunctions
      
!Variable Declarations
Implicit None !force explicit declaration of local variables
Double Precision Time,Timestep
Integer CurrentUnit,CurrentType

!-----------------------------------------------------------------------------------------------------------------------
!User Declarations
Integer NX_Max,NY_Max,NZ_Max,N_Pipes_Max,N_Fluid_Max,N_Layers_Soil_Max,N_Layers_HX_Max
Parameter (NX_Max=100,NY_Max=350,NZ_Max=75,N_Pipes_Max=150,N_Fluid_Max=100,N_Layers_Soil_Max=10,N_Layers_HX_Max=10)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Declarations & Definitions for the User Variables
Double Precision  L_Pipe,D_Pipe_i,D_Pipe_o,K_Pipe,R_Contact,Pi,T_Fluid_in,Flow_in,T_Init_fluid,Cp_Fluid, &
   Rho_Fluid,K_Fluid,Mu_Fluid,K_Soil(N_Layers_Soil_Max),Rho_Soil(N_Layers_Soil_Max),Cp_Soil(N_Layers_Soil_Max), &
   T_Ave_Soil,T_Amp_Soil,TimeShift_Soil,Node_Small,Multiplier,Length_Factor,D_DeepEarth,Tsurf_Farfield, &
   Radial_Factor,Volume_PipeNode,Dia_PipeNode,DX(NX_Max),DY(NY_Max),DZ(NZ_max),Ti_Soil(NX_Max,NY_Max,NZ_Max), &
   Tf_Soil(NX_Max,NY_Max,NZ_Max),Tave_Soil(NX_Max,NY_Max,NZ_Max),Ti_Fluid(N_Fluid_Max,N_Pipes_Max), &
   Tf_Fluid(N_Fluid_Max,N_Pipes_Max),Tave_Fluid(N_Fluid_Max,N_Pipes_Max),Alpha_Soil,Time_Year,Depth,Depth_Now, &
   T_Depth,Tave_Vert(NZ_Max),T_Deep,Tsurf_Kusuda,Reynolds,Prandtl,Nusselt,H_Inner,SA_Inner,Resistance_Fluid, &
   Resistance_Wall,Radius_Node,Radius_Inner,Radius_Outer,Resistance_Radial_i,Q1_Tot,Resistance_Radial_o, &
   UA_Radial(N_Layers_HX_Max),Mass_FluidNode,X_Outlet(N_Pipes_Max),Q_Env(N_Pipes_Max),T_In_Node,BB_Fluid,T_Fluid_Out, &
   Tsurf_o(NX_Max,NY_Max),AA_Soil,BB_Soil,R_Cond_1,R_Cond_2,R_Cond_T,UA_Cond_T,T_Cond,Q2_Tot,A_Cond,Q3_Tot, &
   Cap_Soil(NX_Max,NY_Max,NZ_Max),Tnew_Soil(NX_Max,NY_Max,NZ_Max),Tolerance,L_Between,Depth_Pipe(N_Layers_HX_Max), &
   Tnew_Vert(NZ_Max),R_Cover,Depth_Wash,Thick_Wash,Cap_Node,R_Vertical,L_Extension_Left,Qfront(NX_Max,NY_Max,NZ_Max), &
   Qback(NX_Max,NY_Max,NZ_Max),Qleft(NX_Max,NY_Max,NZ_Max),Qright(NX_Max,NY_Max,NZ_Max),Qtop(NX_Max,NY_Max,NZ_Max), &
   Qbottom(NX_Max,NY_Max,NZ_Max),Qstore(NX_Max,NY_Max,NZ_Max),Balance(NX_Max,NY_Max,NZ_Max),Qpipe(NX_Max,NY_Max,NZ_Max), &
   Qstore_T,Qstore_f,Hconv_Ambient,T_Ambient,Hconv_Zone,T_Zone,R_Conduction,R_Surface,L_Extension_Right,Ti_Vert(NZ_Max), &
   Tf_Vert(NZ_Max),Mdot_Groundwater_x,Mdot_Groundwater_y,Cp_Groundwater,Rho_Groundwater,W_Extension_Back,Velocity_Wash_x, &
   Velocity_Wash_y,Depth_Insul,Q_Pipes,R_Cond_3,Q_boundary,Q_groundwater_x,Q_groundwater_y,Q_top,Q_bottom,EB_Top,EB_Bottom, &
   EB_Error,Q_Fluid,EB_Error_f,Q_temp,Tsurf_i(NX_Max,NY_Max),Area_1,Area_Tot,Tave_Surf_i,Tave_Surf_o,Tave_Surf_f,Q_top_ins, &
   Q1,Q2,Q3,Pipe_Spacing_H,L_Farfield_Left,L_Farfield_Right,L_Farfield_Front,L_Farfield_Back,Thickness_Soil(N_Layers_Soil_Max), &
   End_SoilLayer,Start_PipeLayer,End_PipeLayer,Start_SoilLayer,Z_Previous,Z_DeepEarth,Z_Pipe,Z_Soil,Z_TopWash,Z_BottomWash, &
   Z_Minimum,Emissivity_Soil,Absorptance_Soil,Emissivity_Insul,Absorptance_Insul,Trad_Zone,Qrad_Zone,Tsurface_Zone,T_Sky, &
   Q_Solar,Tsurface_Soil,AA_Fluid,W_Extension_Front,Tsurface_Previous,Hrad_Soil,h_Radiation,Hrad_Zone,K_Above,K_Below, &
   Q_PipetoSoil(N_Pipes_Max,N_Fluid_Max),Balance_Pipe,Qgroundwater(NX_Max,NY_Max,NZ_Max),Qconvection_near,Qshortwave_near, &
   Qlongwave_near,Qconduction_near,Qconvection_zone,Qshortwave_zone,Qlongwave_zone,Qconduction_zone
Integer i,j,k,m,n,nn,p,N_Items,N_Pipes,NX_Beyond_L,NX_Beyond_R,IERR,NY_Beyond_F,NX_Tot,NY_Tot,Inlet(N_Pipes_Max), &
   NY_Beyond_B,NZ_Tot,N_Fluid,NY_Between,IDirection(N_Pipes_Max),NY_Temp,N_Inlets,N_Outlets,Iters_Surface, &
   Order(N_Pipes_Max),I_Found,J_Found,I_Start,I_Stop,I_Step,I_Node,J_Node,K_Node,I_Pipe,Iters,Max_Iters,NX_Insul_L, &
   NX_Insul_R,NY_Insul_F,NY_Insul_B,NZ_toWash,NZ_Wash,Mode_Zone,Mode_Field,LU_OutputFile,NZ_Insul,N_Pipes_Layer,N_Layers_HX, &
   NZ_Now,I_Layer,J_Layer,J_Pipe,K_Layer,N_layers_Soil,I_Soil,N_Start,NZ_Pipe(N_Layers_HX_Max),NZ_Soil(N_Layers_Soil_Max), &
   BoundaryMode_Left,BoundaryMode_Right,BoundaryMode_Front,BoundaryMode_Back,BoundaryMode_Bottom,Iters_Vert
Character (LEN=maxMessageLength) MessageL,MessageX,MessageY,MessageZ,BadFileMessage,NonConvWarning
Character (len=maxPathLength) InputFileName
Logical Have_Pipe(0:NX_Max+1,0:NY_Max+1,0:NZ_Max+1),Converged,Have_FrontInsul(NX_Max,NY_Max,NZ_Max), &
   Have_BackInsul(NX_Max,NY_Max,NZ_Max),Have_LeftInsul(NX_Max,NY_Max,NZ_Max),Have_RightInsul(NX_Max,NY_Max,NZ_Max)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
Data Pi/3.141592654/,Max_Iters/1000/,Cp_Groundwater/4.190/,Rho_Groundwater/1000./
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
Data MessageL/'An illegal pipe layout has been specIfied.  Please make sure that the number of inlets and outlets is the same and non-zero.'/
Data MessageX/'The maximum number of nodes in the x-direction has been exceeded.  Either reduce the number of nodes in the axial direction, increase the minimum node size, or increase the limit on the number of nodes within the source code.'/
Data MessageY/'The maximum number of nodes in the y-direction has been exceeded.  Either reduce the number of nodes in the axial direction, increase the minimum node size, or increase the limit on the number of nodes within the source code.'/
Data MessageZ/'The maximum number of nodes in the z-direction has been exceeded.  Either reduce the number of nodes in the axial direction, increase the minimum node size, or increase the limit on the number of nodes within the source code.'/
Data BadFileMessage/'The elevated soil temperature thermal model was unable to write to the specified output file.  Please make sure that this file is not open in another application.'/
Data NonConvWarning/'The maximum number of internal iterations has been reached without convergence.  Check the energy balance outputs and loosen the tolerances if needed or increase the internal iteration limit.'/
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Global Trnsys Simulation Variables
 Time = getSimulationTime()
 Timestep = getSimulationTimeStep()
 CurrentUnit = getCurrentUnit()
 CurrentType = getCurrentType()
 
!Set the Version Number for This Type
 If (getIsVersionSigningTime()) Then
     Call SetTypeVersion(17)
     Return
 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the Last Call Manipulations Here
 If (getIsLastCallofSimulation()) Then
    N_Pipes_Layer = JFIX(getParameterValue(1)+0.5)
    N_Layers_HX = JFIX(getParameterValue(2)+0.5)
    N_Layers_Soil = JFIX(getParameterValue(3)+0.5)
    N_Fluid = JFIX(getParameterValue(4)+0.5)
    LU_OutputFile = JFIX(getParameterValue(44+N_Layers_HX+4*N_Layers_Soil)+0.5)
    N_Pipes = N_Pipes_Layer*N_Layers_HX

    N_Items = 16+N_Layers_HX+N_Layers_Soil

    NX_Tot = JFIX(getStaticArrayValue(1)+0.5)
    NY_Tot = JFIX(getStaticArrayValue(2)+0.5)
    NZ_Tot = JFIX(getStaticArrayValue(3)+0.5)
    NX_Beyond_L = JFIX(getStaticArrayValue(4)+0.5)
    NX_Insul_L = JFIX(getStaticArrayValue(5)+0.5)
    NX_Insul_R = JFIX(getStaticArrayValue(6)+0.5)
    NX_Beyond_R = JFIX(getStaticArrayValue(7)+0.5)
    NY_Beyond_B = JFIX(getStaticArrayValue(8)+0.5)
    NY_Insul_B = JFIX(getStaticArrayValue(9)+0.5)
    NY_Between = JFIX(getStaticArrayValue(10)+0.5)
    NY_Insul_F = JFIX(getStaticArrayValue(11)+0.5)
    NY_Beyond_F = JFIX(getStaticArrayValue(12)+0.5)
    NZ_toWash = JFIX(getStaticArrayValue(13)+0.5)
    NZ_Wash = JFIX(getStaticArrayValue(14)+0.5)
    NZ_Insul = JFIX(getStaticArrayValue(15)+0.5)
    NZ_Tot = JFIX(getStaticArrayValue(16)+0.5)

    Do j = 1,N_Layers_HX
       NZ_Pipe(j )= JFIX(getStaticArrayValue(16+j)+0.5)
    EndDo

    Do j = 1,N_Layers_Soil
       NZ_Soil(j) = JFIX(getStaticArrayValue(16+N_Layers_HX+j)+0.5)
    EndDo

    Do i = 1,NX_Tot
       DX(i) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+i)
    EndDo

    Do j = 1,NY_Tot
       DY(j) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+j)
    EndDo

    Do k = 1,NZ_Tot
       DZ(k) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+k)
    EndDo

    N_Items=16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+NX_Tot*NY_Tot*NZ_Tot

    Do k = 1,NZ_Tot
       Do j = 1,NY_Tot
          Do i = 1,NX_Tot
             Tf_Soil(i,j,k) = getStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i)
          EndDo
       EndDo
    EndDo
             
    N_Items = N_Items+NX_Tot*NY_Tot*NZ_Tot

    Do n = 1,N_Pipes
       Do m = 1,N_Fluid
          Tf_Fluid(m,n) = getStaticArrayValue(N_Items+N_Fluid*N_Pipes+(n-1)*N_Fluid+m)
       EndDo
    EndDo

    Write(LU_OutputFile,*,ERR=10) NX_Tot,NY_Tot,NZ_Tot
    Write(LU_OutputFile,2500,ERR=10) (DX(i),i=1,NX_Tot)
    Write(LU_OutputFile,2500,ERR=10) (DY(j),j=1,NY_Tot)
    Write(LU_OutputFile,2500,ERR=10) (DZ(k),k=1,NZ_Tot)

    Do k = 1,NZ_Tot
       Write(LU_OutputFile,*) 'Vertical Layer =',k
       Do j = NY_Tot,1,-1
          Write(LU_OutputFile,2500,ERR=10) (Tf_Soil(i,j,k),i = 1,NX_Tot)
       EndDo
    EndDo

    Return

    10 Call Messages(-1,BadFileMessage,'Fatal',CurrentUnit,CurrentType)
    Return

 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Perform Any "After Convergence" Manipulations That May Be Required
 If (getIsEndOfTimestep()) Then
    N_Pipes_Layer = JFIX(getParameterValue(1)+0.5)
    N_Layers_HX = JFIX(getParameterValue(2)+0.5)
    N_Layers_Soil = JFIX(getParameterValue(3)+0.5)
    N_Fluid = JFIX(getParameterValue(4)+0.5)
    N_Pipes = N_Pipes_Layer*N_Layers_HX

    N_Items = 16+N_Layers_HX+N_Layers_Soil

    NX_Tot = JFIX(getStaticArrayValue(1)+0.5)
    NY_Tot = JFIX(getStaticArrayValue(2)+0.5)
    NZ_Tot = JFIX(getStaticArrayValue(3)+0.5)
    NX_Beyond_L = JFIX(getStaticArrayValue(4)+0.5)
    NX_Insul_L = JFIX(getStaticArrayValue(5)+0.5)
    NX_Insul_R = JFIX(getStaticArrayValue(6)+0.5)
    NX_Beyond_R = JFIX(getStaticArrayValue(7)+0.5)
    NY_Beyond_B = JFIX(getStaticArrayValue(8)+0.5)
    NY_Insul_B = JFIX(getStaticArrayValue(9)+0.5)
    NY_Between = JFIX(getStaticArrayValue(10)+0.5)
    NY_Insul_F = JFIX(getStaticArrayValue(11)+0.5)
    NY_Beyond_F = JFIX(getStaticArrayValue(12)+0.5)
    NZ_toWash = JFIX(getStaticArrayValue(13)+0.5)
    NZ_Wash = JFIX(getStaticArrayValue(14)+0.5)
    NZ_Insul = JFIX(getStaticArrayValue(15)+0.5)
    NZ_Tot = JFIX(getStaticArrayValue(16)+0.5)

    Do j = 1,N_Layers_HX
       NZ_Pipe(j )= JFIX(getStaticArrayValue(16+j)+0.5)
    EndDo

    NZ_Soil = NZ_Max
    Do j = 1,N_Layers_Soil
       NZ_Soil(j) = JFIX(getStaticArrayValue(16+N_Layers_HX+j)+0.5)
    EndDo

    N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+2*NX_Tot*NY_Tot*NZ_Tot+2*N_Fluid*N_Pipes+2*NZ_Tot
   
    N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot

!  Update the stored variables as this timestep is done
    Do k = 1,NZ_Tot
       Do j = 1,NY_Tot
          Do i = 1,NX_Tot
             Call SetStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i,getStaticArrayValue(N_Items+NX_Tot*NY_Tot*NZ_Tot+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i))
          EndDo
       EndDo
    EndDo
             
    N_Items = N_Items+2*NX_Tot*NY_Tot*NZ_Tot

    Do n = 1,N_Pipes
       Do m = 1,N_Fluid
          Call SetStaticArrayValue(N_Items+(n-1)*N_Fluid+m,getStaticArrayValue(N_Items+N_Pipes*N_Fluid+(n-1)*N_Fluid+m))
       EndDo
    EndDo

    N_Items=N_Items+2*N_Fluid*N_Pipes

    Do k=1,NZ_Tot
       Call SetStaticArrayValue(N_Items+k,getStaticArrayValue(N_Items+NZ_Tot+k))
    EndDo

    Return
 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the "Very First Call of the Simulation Manipulations" Here
 If (getIsFirstCallofSimulation()) Then

!  Get the critical parameters and check them
    N_Pipes_Layer = JFIX(getParameterValue(1)+0.5)
    If (N_Pipes_Layer <= 0) Call FoundBadParameter(1,'Fatal','The number of pipes in each layer must be greater than 0.')

    N_Layers_HX = JFIX(getParameterValue(2)+0.5)
    If (N_Layers_HX <= 0) Call FoundBadParameter(2,'Fatal','The number of layers of piping must be greater than 0.')
    If (N_Layers_HX > N_Layers_HX_Max) Call FoundBadParameter(2,'Fatal','The number of layers of piping is greater than the maximum number.')

    N_Layers_Soil = JFIX(getParameterValue(3)+0.5)
    If (N_Layers_Soil <= 0) Call FoundBadParameter(3,'Fatal','The number of layers of soil must be greater than 0.')
    If (N_Layers_Soil > N_Layers_Soil_Max) Call FoundBadParameter(3,'Fatal','The number of layers of soil is greater than the maximum number.')

    N_Pipes = N_Pipes_Layer*N_Layers_HX
    If (N_Pipes > N_Pipes_Max) Call FoundBadParameter(1,'Fatal','The total number of pipes (number of pipes per layer times the number of piping layers) is greater than the maximum number.')

  !  Tell the TRNSYS Engine How This Type Works
    Call SetNumberofParameters(49+4*N_Layers_Soil+N_Layers_HX+2*N_Pipes)           
    Call SetNumberofInputs(14)           
    Call SetNumberofDerivatives(0)           
    Call SetNumberofOutputs(25)           
    Call SetIterationMode(1)           

!  Set the Correct Input and Output Variable Types
    Call SetInputUnits(1,'TE1')    
    Call SetInputUnits(2,'MF1')    
    Call SetInputUnits(3,'TE1')    
    Call SetInputUnits(4,'HT1')    
    Call SetInputUnits(5,'TE1')    
    Call SetInputUnits(6,'IR1')    
    Call SetInputUnits(7,'TE1')    
    Call SetInputUnits(8,'TE1')    
    Call SetInputUnits(9,'HT1')    
    Call SetInputUnits(10,'TE1')    
    Call SetInputUnits(11,'IR1')    
    Call SetInputUnits(12,'TE1')    
    Call SetInputUnits(13,'VE1')    
    Call SetInputUnits(14,'VE1')    
    Call SetOutputUnits(1,'TE1')    
    Call SetOutputUnits(2,'MF1')    
    Call SetOutputUnits(3,'DM1')    
    Call SetOutputUnits(4,'PW1')    
    Call SetOutputUnits(5,'PW1')    
    Call SetOutputUnits(6,'PW1')    
    Call SetOutputUnits(7,'PW1')    
    Call SetOutputUnits(8,'PW1')    
    Call SetOutputUnits(9,'PW1')    
    Call SetOutputUnits(10,'PW1')    
    Call SetOutputUnits(11,'PW1')    
    Call SetOutputUnits(12,'PW1')    
    Call SetOutputUnits(13,'PC1')    
    Call SetOutputUnits(14,'PC1')    
    Call SetOutputUnits(15,'TE1')    
    Call SetOutputUnits(16,'TE1')    
    Call SetOutputUnits(17,'TE1')    
    Call SetOutputUnits(18,'PW1')    
    Call SetOutputUnits(19,'PW1')    
    Call SetOutputUnits(20,'PW1')    
    Call SetOutputUnits(21,'PW1')    
    Call SetOutputUnits(22,'PW1')    
    Call SetOutputUnits(23,'PW1')    
    Call SetOutputUnits(24,'PW1')    
    Call SetOutputUnits(25,'PW1')    

!  Return to the TRNSYS Engine
    Return

 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the First Timestep Manipulations Here - There Are No Iterations at the Intial Time
 If (getIsStartTime()) Then

!  Read in the Values of the Parameters and Check for Problems
    N_Pipes_Layer = JFIX(getParameterValue(1)+0.5)
    N_Layers_HX = JFIX(getParameterValue(2)+0.5)
    N_Layers_Soil = JFIX(getParameterValue(3)+0.5)
    N_Fluid = JFIX(getParameterValue(4)+0.5)
    N_Pipes = N_Pipes_Layer*N_Layers_HX

    If (N_Pipes_Layer < 1) Call FoundBadParameter(1,'Fatal','The number of pipes in each layer must be greater than 0.')
    If (N_Layers_HX < 1) Call FoundBadParameter(2,'Fatal','The number of layers of piping must be greater than 0.')
    If (N_Layers_HX > N_Layers_HX_Max) Call FoundBadParameter(2,'Fatal','The number of layers of piping is greater than the maximum number.')
    If (N_Pipes > N_Pipes_Max) Call FoundBadParameter(1,'Fatal','The total number of pipes (number of pipes per layer times the number of piping layers) is greater than the maximum number.')
    If (N_Layers_Soil < 1) Call FoundBadParameter(3,'Fatal','The number of layers of soil must be greater than 0.')
    If (N_Layers_Soil > N_Layers_Soil_Max) Call FoundBadParameter(3,'Fatal','The number of layers of soil is greater than the maximum number.')
    If (N_Fluid < 1) Call FoundBadParameter(4,'Fatal','The number of nodes along the pipe axis must be greater than 0.')
    If (N_Fluid > N_Fluid_Max) Call FoundBadParameter(4,'Fatal','The number of nodes along the pipe axis is greater than the maximum number.')

    If (ErrorFound()) Return

    L_Pipe = getParameterValue(5)
    D_Pipe_i = getParameterValue(6)
    D_Pipe_o = getParameterValue(7)
    K_Pipe = getParameterValue(8)
    R_Contact = getParameterValue(9)
    Pipe_Spacing_H = getParameterValue(10)

    If (L_Pipe <= 0.) Call FoundBadParameter(5,'Fatal','The pipe length must be greater than 0.')
    If (D_Pipe_i <= 0.) Call FoundBadParameter(6,'Fatal','The inside diameter of the pipe must be greater than 0.')
    If (D_Pipe_o <= D_Pipe_i) Call FoundBadParameter(7,'Fatal','The outside diameter of the pipe must be greater than the inside diameter.')
    If (K_Pipe <= 0.) Call FoundBadParameter(8,'Fatal','The thermal conductivity of the pipe must be greater than 0.')
    If (R_Contact < 0.) Call FoundBadParameter(9,'Fatal','The pipe/soil contact resistance cannot be negative.')
    If ((Pipe_Spacing_H <= D_Pipe_o).and.(N_Pipes_Layer>1)) Call FoundBadParameter(10,'Fatal','The horizontal spacing between pipes must be greater than the outside diameter of the pipes.')
    
    Do j = 1,N_Layers_HX
       Depth_Pipe(j) = getParameterValue(10+j)
       If (j == 1) Then
          If (Depth_Pipe(j) <= D_Pipe_o/2.) Call FoundBadParameter(10+j,'Fatal','The depth of the first piping layer must be greater than half the outside diameter of the pipes.')
       Else
          If (Depth_Pipe(j) <= Depth_Pipe(j-1)+D_Pipe_o) Call FoundBadParameter(10+j,'Fatal','The depth of the piping layer must be greater than the depth of the previous layer plus the outside diameter of the pipes.')
       EndIf
    EndDo

    L_Extension_Left = getParameterValue(11+N_Layers_HX)
    L_Extension_Right = getParameterValue(12+N_Layers_HX)
    W_Extension_Front = getParameterValue(13+N_Layers_HX)
    W_Extension_Back = getParameterValue(14+N_Layers_HX)
    R_Cover = getParameterValue(15+N_Layers_HX)
    Depth_Insul = getParameterValue(16+N_Layers_HX)
    R_Vertical = getParameterValue(17+N_Layers_HX)

    If (L_Extension_Left < 0.) Call FoundBadParameter(11+N_Layers_HX,'Fatal','The length of the left surface extension cannot be negative.')
    If (L_Extension_Right < 0.) Call FoundBadParameter(12+N_Layers_HX,'Fatal','The length of the right surface extension cannot be negative.')
    If (W_Extension_Front < 0.) Call FoundBadParameter(13+N_Layers_HX,'Fatal','The length of the front surface extension cannot be negative.')
    If (W_Extension_Back < 0.) Call FoundBadParameter(14+N_Layers_HX,'Fatal','The length of the back surface extension cannot be negative.')
    If (R_Cover < 0.) Call FoundBadParameter(15+N_Layers_HX,'Fatal','The R-value for the surface insulation cannot be negative.')
    If (Depth_Insul < 0.) Call FoundBadParameter(16+N_Layers_HX,'Fatal','The approximate depth of perimeter insulation cannot be negative.')
    If (R_Vertical < 0.) Call FoundBadParameter(17+N_Layers_HX,'Fatal','The R-value for the perimeter insulation cannot be negative.')

    Depth_Wash = getParameterValue(18+N_Layers_HX)
    Thick_Wash = getParameterValue(19+N_Layers_HX)
    If (Thick_Wash <= 0.) Depth_Wash = 0.

    If (Depth_Wash < 0.) Call FoundBadParameter(18+N_Layers_HX,'Fatal','The depth below the surface of the groundwater flow region cannot be negative.')
    If (Thick_Wash < 0.) Call FoundBadParameter(19+N_Layers_HX,'Fatal','The thickness of the groundwater flow region cannot be negative.')

    L_Farfield_Left = getParameterValue(20+N_Layers_HX)
    L_Farfield_Right = getParameterValue(21+N_Layers_HX)
    L_Farfield_Front = getParameterValue(22+N_Layers_HX)
    L_Farfield_Back = getParameterValue(23+N_Layers_HX)
    D_DeepEarth = getParameterValue(24+N_Layers_HX)   !Depth below deepest HX layer

    If (L_Farfield_Left <= 0.) Call FoundBadParameter(20+N_Layers_HX,'Fatal','The left surface far-field distance must be greater than 0.')
    If (L_Farfield_Right <= 0.) Call FoundBadParameter(21+N_Layers_HX,'Fatal','The right surface far-field distance must be greater than 0.')
    If (L_Farfield_Front <= 0.) Call FoundBadParameter(22+N_Layers_HX,'Fatal','The front surface far-field distance must be greater than 0.')
    If (L_Farfield_Back <= 0.) Call FoundBadParameter(23+N_Layers_HX,'Fatal','The back surface far-field distance must be greater than 0.')
    If (D_DeepEarth <= 0.) Call FoundBadParameter(24+N_Layers_HX,'Fatal','The distance to deep earth conditions must be greater than 0.')

    BoundaryMode_Left = JFIX(getParameterValue(25+N_Layers_HX)+0.5)    !1=Conductive,2=Adiabatic
    BoundaryMode_Right = JFIX(getParameterValue(26+N_Layers_HX)+0.5)   !1=Conductive,2=Adiabatic
    BoundaryMode_Front = JFIX(getParameterValue(27+N_Layers_HX)+0.5)   !1=Conductive,2=Adiabatic
    BoundaryMode_Back = JFIX(getParameterValue(28+N_Layers_HX)+0.5)    !1=Conductive,2=Adiabatic
    BoundaryMode_Bottom = JFIX(getParameterValue(29+N_Layers_HX)+0.5)  !1=Conductive,2=Adiabatic

    If (BoundaryMode_Left < 1) Call FoundBadParameter(25+N_Layers_HX,'Fatal','The left surface boundary mode cannot be less than 1.')
    If (BoundaryMode_Left > 2) Call FoundBadParameter(25+N_Layers_HX,'Fatal','The left surface boundary mode cannot be greater than 2.')
    If (BoundaryMode_Right < 1) Call FoundBadParameter(26+N_Layers_HX,'Fatal','The right surface boundary mode cannot be less than 1.')
    If (BoundaryMode_Right > 2) Call FoundBadParameter(26+N_Layers_HX,'Fatal','The right surface boundary mode cannot be greater than 2.')
    If (BoundaryMode_Front < 1) Call FoundBadParameter(27+N_Layers_HX,'Fatal','The front surface boundary mode cannot be less than 1.')
    If (BoundaryMode_Front > 2) Call FoundBadParameter(27+N_Layers_HX,'Fatal','The front surface boundary mode cannot be greater than 2.')
    If (BoundaryMode_Back < 1) Call FoundBadParameter(28+N_Layers_HX,'Fatal','The back surface boundary mode cannot be less than 1.')
    If (BoundaryMode_Back > 2) Call FoundBadParameter(28+N_Layers_HX,'Fatal','The back surface boundary mode cannot be greater than 2.')
    If (BoundaryMode_Bottom < 1) Call FoundBadParameter(29+N_Layers_HX,'Fatal','The bottom surface boundary mode cannot be less than 1.')
    If (BoundaryMode_Bottom > 2) Call FoundBadParameter(29+N_Layers_HX,'Fatal','The bottom surface boundary mode cannot be greater than 2.')

    T_Init_Fluid = getParameterValue(30+N_Layers_HX)
    Cp_Fluid = getParameterValue(31+N_Layers_HX)
    Rho_Fluid = getParameterValue(32+N_Layers_HX)
    K_Fluid = getParameterValue(33+N_Layers_HX)
    Mu_Fluid = getParameterValue(34+N_Layers_HX)

    If (Cp_Fluid <= 0.) Call FoundBadParameter(31+N_Layers_HX,'Fatal','The specific heat of the fluid must be greater than 0.')
    If (Rho_Fluid <= 0.) Call FoundBadParameter(32+N_Layers_HX,'Fatal','The density of the fluid must be greater than 0.')
    If (K_Fluid <= 0.) Call FoundBadParameter(33+N_Layers_HX,'Fatal','The thermal conductivity of the fluid must be greater than 0.')
    If (Mu_Fluid <= 0.) Call FoundBadParameter(34+N_Layers_HX,'Fatal','The viscosity of the fluid must be greater than 0.')

    Do j = 1,N_Layers_Soil
       K_Soil(j) = getParameterValue(35+N_Layers_HX+4*(j-1))
       Rho_Soil(j) = getParameterValue(36+N_Layers_HX+4*(j-1))
       Cp_Soil(j) = getParameterValue(37+N_Layers_HX+4*(j-1))
       Thickness_Soil(j) = getParameterValue(38+N_Layers_HX+4*(j-1))

       If (K_Soil(j) <= 0.) Call FoundBadParameter(35+N_Layers_HX+4*(j-1),'Fatal','The thermal conductivity of the soil layer must be greater than 0.')
       If (Rho_Soil(j) <= 0.) Call FoundBadParameter(36+N_Layers_HX+4*(j-1),'Fatal','The density of the soil layer must be greater than 0.')
       If (Cp_Soil(j) <= 0.) Call FoundBadParameter(37+N_Layers_HX+4*(j-1),'Fatal','The specific heat of the soil layer must be greater than 0.')
       If (Thickness_Soil(j) <= 0.) Call FoundBadParameter(38+N_Layers_HX+4*(j-1),'Fatal','The thickness of the soil layer must be greater than 0.')
    EndDo

    T_Ave_Soil = getParameterValue(35+N_Layers_HX+4*N_Layers_Soil)
    T_Amp_Soil = getParameterValue(36+N_Layers_HX+4*N_Layers_Soil)
    TimeShift_Soil = getParameterValue(37+N_Layers_HX+4*N_Layers_Soil)

    If (T_Amp_Soil < 0.) Call FoundBadParameter(36+N_Layers_HX+4*N_Layers_Soil,'Fatal','The amplitude of the surface temperature cannot be less than 0.')
    If (TimeShift_Soil < -365.) Call FoundBadParameter(37+N_Layers_HX+4*N_Layers_Soil,'Fatal','The day of minimum surface temperature cannot be less than -365.')

    Node_Small = getParameterValue(38+N_Layers_HX+4*N_Layers_Soil)
    Multiplier = getParameterValue(39+N_Layers_HX+4*N_Layers_Soil)
    Length_Factor = getParameterValue(40+N_Layers_HX+4*N_Layers_Soil)
    Radial_Factor = getParameterValue(41+N_Layers_HX+4*N_Layers_Soil)

    If (Node_Small <= 0.) Call FoundBadParameter(38+N_Layers_HX+4*N_Layers_Soil,'Fatal','The size of the smallest node must be greater than 0.')
    If (Multiplier < 1.) Call FoundBadParameter(39+N_Layers_HX+4*N_Layers_Soil,'Fatal','The node size multiplier cannot be less than 1.')
    If (Length_Factor <= 0.) Call FoundBadParameter(40+N_Layers_HX+4*N_Layers_Soil,'Fatal','The pipe length factor must be greater than 0.')
    If (Radial_Factor < 1.) Call FoundBadParameter(41+N_Layers_HX+4*N_Layers_Soil,'Fatal','The pipe diameter node size multiplier cannot be less than 1.')

    Mode_Zone = JFIX(getParameterValue(42+N_Layers_HX+4*N_Layers_Soil)+0.5)  !1=Energy Balance, 2=Kusuda, 3=Input Value
    Mode_Field = JFIX(getParameterValue(43+N_Layers_HX+4*N_Layers_Soil)+0.5) !1=Energy Balance, 2=Kusuda, 3=Input Value

    If (Mode_Zone < 1) Call FoundBadParameter(42+N_Layers_HX+4*N_Layers_Soil,'Fatal','The surface temperature calculation mode for the surface directly above the pipes cannot be less than 1.')
    If (Mode_Zone > 3) Call FoundBadParameter(42+N_Layers_HX+4*N_Layers_Soil,'Fatal','The surface temperature calculation mode for the surface directly above the pipes cannot be greater than 3.')

    If (Mode_Field < 1) Call FoundBadParameter(43+N_Layers_HX+4*N_Layers_Soil,'Fatal','The surface temperature calculation mode for the soil surface cannot be less than 1.')
    If (Mode_Field > 3) Call FoundBadParameter(43+N_Layers_HX+4*N_Layers_Soil,'Fatal','The surface temperature calculation mode for the soil surface cannot be greater than 3.')

    LU_OutputFile = JFIX(getParameterValue(44+N_Layers_HX+4*N_Layers_Soil)+0.5)

    If ((LU_OutputFile > 0).and.(LU_OutputFile < 10)) Call FoundBadParameter(44+N_Layers_HX+4*N_Layers_Soil,'Fatal','The logical unit number for the output data file cannot be between 0 and 10.')
    InputFileName = getLUFilename(LU_OutputFile)
    Open (unit=LU_OutputFile,file=InputFileName,status='UNKNOWN')

    Tolerance = getParameterValue(45+N_Layers_HX+4*N_Layers_Soil)

    If (Tolerance <= 0.) Call FoundBadParameter(45+N_Layers_HX+4*N_Layers_Soil,'Fatal','The convergence tolerance must be greater than 0.')

    Emissivity_Soil = getParameterValue(46+N_Layers_HX+4*N_Layers_Soil)
    Absorptance_Soil = getParameterValue(47+N_Layers_HX+4*N_Layers_Soil)
    Emissivity_Insul = getParameterValue(48+N_Layers_HX+4*N_Layers_Soil)
    Absorptance_Insul = getParameterValue(49+N_Layers_HX+4*N_Layers_Soil)

    If (Emissivity_Soil < 0.) Call FoundBadParameter(46+N_Layers_HX+4*N_Layers_Soil,'Fatal','The emissivity of the soil cannot be less than 0.')
    If (Emissivity_Soil > 1.) Call FoundBadParameter(46+N_Layers_HX+4*N_Layers_Soil,'Fatal','The emissivity of the soil cannot be greater than 1.')
    If (Absorptance_Soil < 0.) Call FoundBadParameter(47+N_Layers_HX+4*N_Layers_Soil,'Fatal','The absorptance of the soil cannot be less than 0.')
    If (Absorptance_Soil > 1.) Call FoundBadParameter(47+N_Layers_HX+4*N_Layers_Soil,'Fatal','The absorptance of the soil cannot be greater than 1.')
    If (Emissivity_Insul < 0.) Call FoundBadParameter(48+N_Layers_HX+4*N_Layers_Soil,'Fatal','The emissivity of the soil directly above the pipes cannot be less than 0.')
    If (Emissivity_Insul > 1.) Call FoundBadParameter(48+N_Layers_HX+4*N_Layers_Soil,'Fatal','The emissivity of the soil directly above the pipes cannot be greater than 1.')
    If (Absorptance_Insul < 0.) Call FoundBadParameter(49+N_Layers_HX+4*N_Layers_Soil,'Fatal','The absorptance of the soil directly above the pipes cannot be less than 0.')
    If (Absorptance_Insul > 1.) Call FoundBadParameter(49+N_Layers_HX+4*N_Layers_Soil,'Fatal','The absorptance of the soil directly above the pipes cannot be greater than 1.')

    If (ErrorFound()) Return

   !Get the piping layout
    Do n = 1,N_Pipes
       Inlet(n) = JFIX(getParameterValue(50+N_Layers_HX+4*N_Layers_Soil+2*(n-1))+0.5)        !0=Use input value, n=Use that pipe's outlet
       IDirection(n) = JFIX(getParameterValue(51+N_Layers_HX+4*N_Layers_Soil+2*(N-1))+0.5)   !1=Standard direction, 2=Reverse direction

       If (Inlet(n) < 0) Call FoundBadParameter(50+N_Layers_HX+4*N_Layers_Soil+2*(n-1),'Fatal','The inlet value for the pipe cannot be less than 0.')
       If (Inlet(n) > N_Pipes) Call FoundBadParameter(50+N_Layers_HX+4*N_Layers_Soil+2*(n-1),'Fatal','The inlet value for the pipe cannot be greater than number of pipes.')
       If (Inlet(n) == n) Call FoundBadParameter(50+N_Layers_HX+4*N_Layers_Soil+2*(n-1),'Fatal','The inlet value for the pipe cannot be the same as the pipe number.')

       If (IDirection(n) < 1) Call FoundBadParameter(51+N_Layers_HX+4*N_Layers_Soil+2*(n-1),'Fatal','The pipe direction parameter cannot be less than 1.')
       If (IDirection(n) > 2) Call FoundBadParameter(51+N_Layers_HX+4*N_Layers_Soil+2*(n-1),'Fatal','The pipe direction parameter cannot be greater than 2.')
    EndDo
    If (ErrorFound()) Return

   !Check the number of inlets and outlets for problems
    N_Inlets = 0
    Do n = 1,N_Pipes
       If (Inlet(n) == 0) N_Inlets = N_Inlets+1
    EndDo

    X_Outlet = 1.
    N_Outlets = 0
    Do n = 1,N_Pipes
       Do nn = 1,N_Pipes
          If (n == Inlet(nn)) Then
             N_Outlets = N_Outlets+1
             X_Outlet(n) = 0.
          EndIf
       EndDo
    EndDo
    N_Outlets = N_Pipes-N_Outlets

    If ((N_Inlets <= 0).OR.(N_Outlets /= N_Inlets)) Then
       Call Messages(-1,MessageL,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

   !Order the pipes for the best calling Order
    I_Found = 0
    Do I = 1,N_Pipes
       If (Inlet(I) == 0) Then
          Order(I_Found+1) = I
          I_Found = I_Found+1
          5 J_Found = 0
          Do J = 1,N_Pipes
             If (Inlet(J) == Order(I_Found)) Then
                J_Found = J_Found+1
                Order(I_Found+1) = J
                I_Found = I_Found+1
                Exit
             EndIf
          EndDo
          If (J_Found > 0) Goto 5
       EndIf
    EndDo

    If (I_Found /= N_Pipes) Then
       Call Messages(-1,MessageL,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

!  Set the sizes of the nodes in the x-direction beyond the edges of the insulated surface (along the pipe axis)
    Call Node_LargetoSmall(NX_Max,Node_Small,Multiplier,L_Farfield_Left,DX,1,NX_Beyond_L,IERR)
    If (IERR > 0) Then
       Call Messages(-1,MessageX,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

!  Set the sizes of the nodes in the x-direction from the edge of the insulated surface to the start of the pipes
    If (L_Extension_Left > 0.) THEN
       Call Node_SmalltoSmall(NX_Max,Node_Small,Multiplier,L_Extension_Left,DX,NX_Beyond_L+1,NX_Insul_L,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageX,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf
    Else
       NX_Insul_L = 0
    EndIf

!  Set the sizes of the nodes along the length of the Pipe
    If ((N_Fluid+NX_Beyond_L+NX_Insul_L) > NX_Max) Then
       Call Messages(-1,MessageX,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

    Do i = 1,N_Fluid
       DX(NX_Beyond_L+NX_Insul_L+i) = L_Pipe/DBLE(N_Fluid)
    EndDo

!  Set the sizes of the nodes in the x-direction beyond the edges of the pipes to the insulated boundary
    If (L_Extension_Right > 0.) THEN
       Call Node_SmalltoSmall(NX_Max,Node_Small,Multiplier,L_Extension_Right,DX,NX_Beyond_L+NX_Insul_L+N_Fluid+1,NX_Insul_R,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageX,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf
    Else
       NX_Insul_R = 0
    EndIf

!  Set the sizes of the nodes in the x-direction beyond the edges of the insulated boundary
    Call Node_SmalltoLarge(NX_Max,Node_Small,Multiplier,L_Farfield_Right,DX,NX_Beyond_L+NX_Insul_L+N_Fluid+NX_Insul_R+1,NX_Beyond_R,IERR)
    If (IERR > 0) Then
       Call Messages(-1,MessageX,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

!  Set the Pipe indicators
    NX_Tot = NX_Beyond_L+NX_Insul_L+N_Fluid+NX_Insul_R+NX_Beyond_R

! ****************

!  Set the sizes of the nodes in the y-direction beyond the edges of the Pipes 
    Call Node_LargetoSmall(NY_Max,Node_Small,Multiplier,L_Farfield_Back,DY,1,NY_Beyond_B,IERR)
    If (IERR > 0) Then
       Call Messages(-1,MessageY,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

!  Set the sizes of the nodes in the y-direction from the edge of the insulated surface to the start of the pipes
    If (W_Extension_Back > 0.) Then
       Call Node_SmalltoSmall(NY_Max,Node_Small,Multiplier,W_Extension_Back,DY,NY_Beyond_B+1,NY_Insul_B,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageY,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf
    Else
       NY_Insul_B = 0
    EndIf

   !Set the size of the node containing the 1st Pipe
    DY(NY_Beyond_B+NY_Insul_B+1) = Radial_Factor*D_Pipe_o
    NY_Temp = NY_Beyond_B+NY_Insul_B+1
    NY_Between = 0

   !Calculate the distance between pipe nodes
    L_Between = Pipe_Spacing_H-Radial_Factor*D_Pipe_o
    If (L_Between <= 0.) Then
       Call Messages(-1,'The distance betwen pipes in the horizontal direction is not sufficient to cover the distnce betwen the pipes when the radial node factor is applied.' ,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

   !Set the sizes of the nodes between the pipes
    Do n = 1,N_Pipes_Layer-1

      !Set the sizes of the nodes in the y-direction between the Pipes 
       Call Node_SmalltoSmall(NY_Max,Node_Small,Multiplier,L_Between,DY,NY_Temp+1,NY_Between,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageY,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf

      !Set the size of the node containing the Pipe
       DY(NY_Temp+NY_Between+1) = Radial_Factor*D_Pipe_o

       NY_Temp = NY_Temp+NY_Between+1

    EndDo

!  Set the sizes of the nodes in the y-direction beyond the edges of the Pipes to the edge of the insulation
    If (W_Extension_Front > 0.) Then
       Call Node_SmalltoSmall(NY_Max,Node_Small,Multiplier,W_Extension_Front,DY,NY_Temp+1,NY_Insul_F,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageY,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf
    Else
       NY_Insul_F = 0
    EndIf

!  Set the sizes of the nodes in the y-direction beyond the edges of the Pipes 
    Call Node_SmalltoLarge(NY_Max,Node_Small,Multiplier,L_Farfield_Front,DY,NY_Temp+NY_Insul_F+1,NY_Beyond_F,IERR)
    If (IERR > 0) Then
       Call Messages(-1,MessageY,'Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

!  Set the Pipe indicators
    NY_Tot = NY_Beyond_B+NY_Insul_B+N_Pipes_Layer+(N_Pipes_layer-1)*NY_Between+NY_Insul_F+NY_Beyond_F

! ****************
   
   !Check the vertical distances for problems with the layout
    If (Depth_Pipe(1) <= Radial_Factor*D_Pipe_o/2.) Then
       Call Messages(-1,'The depth of the first pipe layer is not sufficient given the size of the radial factor parameters and the diameter of the pipe.','Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

    End_SoilLayer = 0.
    Do i = 1,N_Layers_Soil
       End_SoilLayer = End_SoilLayer+Thickness_Soil(i)
    EndDo
    If (End_SoilLayer < (Depth_Pipe(N_Layers_HX)+Radial_Factor*D_Pipe_o/2.+D_DeepEarth)) Then
       Call Messages(-1,'The total depth of the soil layers is not sufficient to cover the depth of the system.','Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

    If (Depth_Pipe(1) <= Radial_Factor*D_Pipe_o/2.) Then
       Call Messages(-1,'The depth of the first pipe layer is not sufficient given the size of the radial factor parameters and the diameter of the pipe.','Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

    Do i = 1,N_Layers_HX
       Start_PipeLayer = Depth_Pipe(i)-Radial_Factor*D_Pipe_o/2.
       End_PipeLayer = Depth_Pipe(i)+Radial_Factor*D_Pipe_o/2.
       Start_SoilLayer = 0.

       Do j = 1,N_Layers_Soil
          End_SoilLayer = Start_SoilLayer+Thickness_Soil(j)

          If ((Start_SoilLayer >= Start_PipeLayer).and.(Start_SoilLayer <= End_PipeLayer)) Then
             Call Messages(-1,'One or more of the soil layer boundaries conflict with the location of a buried pipe layer.','Fatal',CurrentUnit,CurrentType)
             Return
          EndIf

          If ((End_SoilLayer >= Start_PipeLayer).and.(End_SoilLayer <= End_PipeLayer)) Then
             Call Messages(-1,'One or more of the soil layer boundaries conflict with the location of a buried pipe layer.','Fatal',CurrentUnit,CurrentType)
             Return
          EndIf

          Start_SoilLayer = Start_SoilLayer+Thickness_Soil(j)
       EndDo
    EndDo

    Do i = 1,N_Layers_HX
       Start_PipeLayer = Depth_Pipe(i)-Radial_Factor*D_Pipe_o/2.
       End_PipeLayer = Depth_Pipe(i)+Radial_Factor*D_Pipe_o/2.

       Do j = 1,N_Layers_Soil

          If ((Depth_Wash >= Start_PipeLayer).and.(Depth_Wash <= End_PipeLayer)) Then
             Call Messages(-1,'The top of the wash region conflicts with the location of a buried pipe layer.','Fatal',CurrentUnit,CurrentType)
             Return
          EndIf

          If (((Depth_Wash+Thick_Wash) >= Start_PipeLayer).and.((Depth_Wash+Thick_Wash) <= End_PipeLayer)) Then
             Call Messages(-1,'The bottom of the wash region conflicts with the location of a buried pipe layer.','Fatal',CurrentUnit,CurrentType)
             Return
          EndIf

          Start_SoilLayer = Start_SoilLayer+Thickness_Soil(j)
       EndDo
    EndDo

   !Find the distance down to the next edge
    I_Pipe = 1
    I_Soil = 1
    Z_Previous = 0.
    N_Start = 0
    NZ_toWash = 0
    NZ_Wash = 0
    NZ_Pipe = 0
    NZ_Soil = NZ_Max

    Z_DeepEarth = Depth_Pipe(N_Layers_HX)+Radial_Factor*D_Pipe_o/2.+D_DeepEarth

    If (Depth_Wash > Z_DeepEarth) Then
       Depth_Wash = 0.
       Thick_Wash = 0.
    EndIf

    Z_Pipe = Depth_Pipe(I_Pipe)-Radial_Factor*D_Pipe_o/2.
    Z_Soil = Thickness_Soil(I_Soil)
    If (Thick_Wash > 0.) Then
       Z_TopWash = Depth_Wash
       Z_BottomWash = Depth_Wash+Thick_Wash
    Else
       Z_TopWash = 1.d+20
       Z_BottomWash = 2.d+20
    EndIf

    50 Z_Minimum = DMIN1(Z_DeepEarth,DMIN1(DMIN1(Z_Pipe,Z_Soil),DMIN1(Z_TopWash,Z_BottomWash)))

    If (Z_Pipe == Z_Minimum) Then
       Call Node_SmalltoSmall(NZ_Max,Node_Small,Multiplier,Z_Minimum-Z_Previous,DZ,N_Start+1,NZ_Now,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageZ,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf

       NZ_Pipe(I_Pipe) = N_Start+NZ_Now+1
       DZ(N_Start+NZ_Now+1) = Radial_Factor*D_Pipe_o
       If (I_Pipe == N_Layers_HX) Then
          I_Pipe = I_Pipe
          N_Start = N_Start+NZ_Now+1
          Z_Pipe = 1.d+20
       Else
          I_Pipe = I_Pipe+1
          N_Start = N_Start+NZ_Now+1
          Z_Pipe = Depth_Pipe(I_Pipe)-Radial_Factor*D_Pipe_o/2.
       EndIf
       Z_Previous = Z_Minimum+Radial_Factor*D_Pipe_o
       Goto 50

    ElseIf (Z_Soil == Z_Minimum) Then
       Call Node_SmalltoSmall(NZ_Max,Node_Small,Multiplier,Z_Minimum-Z_Previous,DZ,N_Start+1,NZ_Now,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageZ,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf

      NZ_Soil(I_Soil) = N_Start+NZ_Now
      If (I_Soil == N_Layers_Soil) Then
         I_Soil = I_Soil
         N_Start = N_Start+NZ_Now
         Z_Soil = 1.d+20
      Else
         I_Soil = I_Soil+1
         N_Start = N_Start+NZ_Now
         Z_Soil = Z_Soil+Thickness_Soil(I_Soil)
      EndIf
      Z_Previous = Z_Minimum
      Goto 50

    ElseIf (Z_TopWash == Z_Minimum) Then
       Call Node_SmalltoSmall(NZ_Max,Node_Small,Multiplier,Z_Minimum-Z_Previous,DZ,N_Start+1,NZ_Now,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageZ,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf

      NZ_toWash = N_Start+NZ_Now
      N_Start = N_Start+NZ_Now
      Z_TopWash = 1.d+20
      Z_Previous = Z_Minimum
      Goto 50

    ElseIf (Z_BottomWash == Z_Minimum) Then
       Call Node_SmalltoSmall(NZ_Max,Node_Small,Multiplier,Z_Minimum-Z_Previous,DZ,N_Start+1,NZ_Now,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageZ,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf

      NZ_Wash = N_Start+NZ_Now-NZ_toWash
      N_Start = N_Start+NZ_Now
      Z_BottomWash = 1.d+20
      Z_Previous = Z_Minimum
      Goto 50

    ElseIf (Z_DeepEarth == Z_Minimum) Then
       Call Node_SmalltoLarge(NZ_Max,Node_Small,Multiplier,Z_Minimum-Z_Previous,DZ,N_Start+1,NZ_Now,IERR)
       If (IERR > 0) Then
          Call Messages(-1,MessageZ,'Fatal',CurrentUnit,CurrentType)
          Return
       EndIf
       NZ_Tot = N_Start+NZ_Now
       NZ_Soil(I_Soil) = NZ_Tot
       If ((NZ_toWash > 0).and.(NZ_Wash == 0)) NZ_Wash = NZ_Tot-NZ_toWash

    Else
       Call Messages(-1,'An error occurred while noding in the depth direction.','Fatal',CurrentUnit,CurrentType)
       Return
    EndIf

   !Calculate the depth of the edge insulation
    If (Depth_Insul <= 0.) Then
       NZ_Insul = 0
    Else
       Depth_Now = 0.
       Do k = 1,NZ_Tot
         NZ_Insul = k
         Depth_Now = Depth_Now+DZ(k)
         If (Depth_Now >= (Depth_Insul-Node_Small/2.)) Exit
       EndDo
    EndIf
   
   !Set the Size of the Storage Array
    N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+2*NX_Tot*NY_Tot*NZ_Tot+2*N_Fluid*N_Pipes+2*NZ_Tot
    Call SetNumberStoredVariables(16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+2*NX_Tot*NY_Tot*NZ_Tot+2*N_Fluid*N_Pipes+2*NZ_Tot,0)

   !Set the indicator for the nodes which have pipes
    Do i = 1,NX_Tot
       Do j = 1,NY_Tot
          Do k = 1,NZ_Tot
             Have_Pipe(i,j,k) = .False.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+NX_Insul_L+1,NX_Beyond_L+NX_Insul_L+N_Fluid
       Do m = 1,N_Layers_HX
          Do n = 1,N_Pipes_Layer
             NZ_Now = NZ_Pipe(m)
             Do k = NZ_Now,NZ_Now
                j = NY_Beyond_B+NY_Insul_B+n+(n-1)*NY_Between
                Have_Pipe(i,j,k) = .True.
             EndDo
          EndDo
       EndDo
    EndDo

   !Set the indicator for which nodes have vertical surface insulation
    Do i = 1,NX_Tot
       Do j = 1,NY_Tot
          Do k = 1,NZ_Tot
             Have_FrontInsul(i,j,k) = .False.
             Have_BackInsul(i,j,k) = .False.
             Have_LeftInsul(i,j,k) = .False.
             Have_RightInsul(i,j,k) = .False.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
       Do j = NY_Beyond_B,NY_Beyond_B
          Do k = 1,NZ_Insul
             Have_FrontInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
       Do j = NY_Tot-NY_Beyond_F-1,NY_Tot-NY_Beyond_F-1
          Do k = 1,NZ_Insul
             Have_FrontInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
       Do j = NY_Beyond_B+1,NY_Beyond_B+1
          Do k = 1,NZ_Insul
             Have_BackInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
       Do j = NY_Tot-NY_Beyond_F,NY_Tot-NY_Beyond_F
          Do k = 1,NZ_Insul
             Have_BackInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L,NX_Beyond_L
       Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
          Do k = 1,NZ_Insul
             Have_RightInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Tot-NX_Beyond_R-1,NX_Tot-NX_Beyond_R-1
       Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
          Do k = 1,NZ_Insul
             Have_RightInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Beyond_L+1,NX_Beyond_L+1
       Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
          Do k = 1,NZ_Insul
             Have_LeftInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

    Do i = NX_Tot-NX_Beyond_R,NX_Tot-NX_Beyond_R
       Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
          Do k = 1,NZ_Insul
             Have_LeftInsul(i,j,k) = .True.
          EndDo
       EndDo
    EndDo

   !Set the initial values of the stored variables
    Call SetStaticArrayValue(1,DBLE(NX_Tot))
    Call SetStaticArrayValue(2,DBLE(NY_Tot))
    Call SetStaticArrayValue(3,DBLE(NZ_Tot))
    Call SetStaticArrayValue(4,DBLE(NX_Beyond_L))
    Call SetStaticArrayValue(5,DBLE(NX_Insul_L))
    Call SetStaticArrayValue(6,DBLE(NX_Insul_R))
    Call SetStaticArrayValue(7,DBLE(NX_Beyond_R))
    Call SetStaticArrayValue(8,DBLE(NY_Beyond_B))
    Call SetStaticArrayValue(9,DBLE(NY_Insul_B))
    Call SetStaticArrayValue(10,DBLE(NY_Between))
    Call SetStaticArrayValue(11,DBLE(NY_Insul_F))
    Call SetStaticArrayValue(12,DBLE(NY_Beyond_F))
    Call SetStaticArrayValue(13,DBLE(NZ_toWash))
    Call SetStaticArrayValue(14,DBLE(NZ_Wash))
    Call SetStaticArrayValue(15,DBLE(NZ_Insul))
    Call SetStaticArrayValue(16,DBLE(NZ_Tot))

    Do j = 1,N_Layers_HX
       Call SetStaticArrayValue(16+j,DBLE(NZ_Pipe(j)))
    EndDo

    Do j = 1,N_Layers_Soil
       Call SetStaticArrayValue(16+N_Layers_HX+j,DBLE(NZ_Soil(j)))
    EndDo

    Do i = 1,NX_Tot
       Call SetStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+i,DX(i))
    EndDo

    Do j = 1,NY_Tot
       Call SetStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+j,DY(j))
    EndDo

    Do k = 1,NZ_Tot
       Call SetStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+k,DZ(k))
    EndDo

    N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot

   !Set a few soil properties
    Time_Year = Time/24.
    Time_Year = DMOD(Time_Year,365.)

    I_Soil = 1
    Depth_Now = 0.
    Do k = 1,NZ_Tot
       If (k > NZ_Soil(I_Soil)) Then
          I_Soil = I_Soil+1
       EndIf
       
       Alpha_Soil = K_Soil(I_Soil)/Rho_Soil(I_Soil)/CP_Soil(I_Soil)*24.   !m2/day
       
       Do j = 1,NY_Tot
          Do i = 1,NX_Tot
             Depth = Depth_Now+DZ(k)/2.
             T_Depth = T_Ave_Soil-T_Amp_Soil*DEXP(-Depth*((Pi/365./Alpha_Soil)**0.5))*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil-Depth/2.*((365./Pi/Alpha_Soil)**0.5)))
             Call SetStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i,T_Depth)
             Call SetStaticArrayValue(N_Items+NX_Tot*NY_Tot*NZ_Tot+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i,T_Depth)
          EndDo
       EndDo
       Depth_Now = Depth_Now+DZ(k)
    EndDo
             
    N_Items = N_Items+2*NX_Tot*NY_Tot*NZ_Tot

    Do n = 1,N_Pipes
       Do m = 1,N_Fluid
          Call SetStaticArrayValue(N_Items+(n-1)*N_Fluid+m,T_Init_Fluid)
          Call SetStaticArrayValue(N_Items+N_Fluid*N_Pipes+(n-1)*N_Fluid+m,T_Init_Fluid)
       EndDo
    EndDo

    N_Items = N_Items+2*N_Fluid*N_Pipes

    I_Soil = 1
    Depth_Now = 0.
    Do k = 1,NZ_Tot
       If (k > NZ_Soil(I_Soil)) Then
          I_Soil = I_Soil+1
       EndIf
       
       Alpha_Soil = K_Soil(I_Soil)/Rho_Soil(I_Soil)/CP_Soil(I_Soil)*24.   !m2/day

       Depth = Depth_Now+DZ(k)/2.
       T_Depth = T_Ave_Soil-T_Amp_Soil*DEXP(-Depth*((Pi/365./Alpha_Soil)**0.5))*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil-Depth/2.*((365./Pi/Alpha_Soil)**0.5)))
       Call SetStaticArrayValue(N_Items+k,T_Depth)
       Call SetStaticArrayValue(N_Items+NZ_Tot+k,T_Depth)
       Depth_Now = Depth_Now+DZ(k)
    EndDo

    N_Items = N_Items+2*NZ_Tot

   !Calculate the surface temperatures above the field
    Tsurf_Kusuda = T_Ave_Soil-T_Amp_Soil*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil))

   !Set the initial values of the Outputs
    Call SetOutputValue(1,T_Init_Fluid)
    Do k = 2,14
       Call SetOutputValue(k,0.d0)
    EndDo
    Do k = 15,17
       Call SetOutputValue(k,Tsurf_Kusuda)
    EndDo
    Do k = 18,25
       Call SetOutputValue(k,0.d0)
    EndDo

   !Return to the TRNSYS Engine
    Return

EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
 If (getIsReReadParameters()) Then
        
!  Read in the Values of the Parameters
    N_Pipes_Layer = JFIX(getParameterValue(1)+0.5)
    N_Layers_HX = JFIX(getParameterValue(2)+0.5)
    N_Layers_Soil = JFIX(getParameterValue(3)+0.5)
    N_Fluid = JFIX(getParameterValue(4)+0.5)
    N_Pipes = N_Pipes_Layer*N_Layers_HX
    L_Pipe = getParameterValue(5)
    D_Pipe_i = getParameterValue(6)
    D_Pipe_o = getParameterValue(7)
    K_Pipe = getParameterValue(8)
    R_Contact = getParameterValue(9)
    Pipe_Spacing_H = getParameterValue(10)
    Do j = 1,N_Layers_HX
       Depth_Pipe(j) = getParameterValue(10+j)
    EndDo
    L_Extension_Left = getParameterValue(11+N_Layers_HX)
    L_Extension_Right = getParameterValue(12+N_Layers_HX)
    W_Extension_Front = getParameterValue(13+N_Layers_HX)
    W_Extension_Back = getParameterValue(14+N_Layers_HX)
    R_Cover = getParameterValue(15+N_Layers_HX)
    Depth_Insul = getParameterValue(16+N_Layers_HX)
    R_Vertical = getParameterValue(17+N_Layers_HX)
    Depth_Wash = getParameterValue(18+N_Layers_HX)
    Thick_Wash = getParameterValue(19+N_Layers_HX)
    If (Thick_Wash <= 0.) Depth_Wash = 0.
    L_Farfield_Left = getParameterValue(20+N_Layers_HX)
    L_Farfield_Right = getParameterValue(21+N_Layers_HX)
    L_Farfield_Front = getParameterValue(22+N_Layers_HX)
    L_Farfield_Back = getParameterValue(23+N_Layers_HX)
    D_DeepEarth = getParameterValue(24+N_Layers_HX)   !Depth below deepest HX layer
    BoundaryMode_Left = JFIX(getParameterValue(25+N_Layers_HX)+0.5)    !1=Conductive,2=Adiabatic
    BoundaryMode_Right = JFIX(getParameterValue(26+N_Layers_HX)+0.5)   !1=Conductive,2=Adiabatic
    BoundaryMode_Front = JFIX(getParameterValue(27+N_Layers_HX)+0.5)   !1=Conductive,2=Adiabatic
    BoundaryMode_Back = JFIX(getParameterValue(28+N_Layers_HX)+0.5)    !1=Conductive,2=Adiabatic
    BoundaryMode_Bottom = JFIX(getParameterValue(29+N_Layers_HX)+0.5)  !1=Conductive,2=Adiabatic
    T_Init_Fluid = getParameterValue(30+N_Layers_HX)
    Cp_Fluid = getParameterValue(31+N_Layers_HX)
    Rho_Fluid = getParameterValue(32+N_Layers_HX)
    K_Fluid = getParameterValue(33+N_Layers_HX)
    Mu_Fluid = getParameterValue(34+N_Layers_HX)
    Do j = 1,N_Layers_Soil
       K_Soil(j) = getParameterValue(35+N_Layers_HX+4*(j-1))
       Rho_Soil(j) = getParameterValue(36+N_Layers_HX+4*(j-1))
       Cp_Soil(j) = getParameterValue(37+N_Layers_HX+4*(j-1))
       Thickness_Soil(j) = getParameterValue(38+N_Layers_HX+4*(j-1))
    EndDo
    T_Ave_Soil = getParameterValue(35+N_Layers_HX+4*N_Layers_Soil)
    T_Amp_Soil = getParameterValue(36+N_Layers_HX+4*N_Layers_Soil)
    TimeShift_Soil = getParameterValue(37+N_Layers_HX+4*N_Layers_Soil)
    Node_Small = getParameterValue(38+N_Layers_HX+4*N_Layers_Soil)
    Multiplier = getParameterValue(39+N_Layers_HX+4*N_Layers_Soil)
    Length_Factor = getParameterValue(40+N_Layers_HX+4*N_Layers_Soil)
    Radial_Factor = getParameterValue(41+N_Layers_HX+4*N_Layers_Soil)
    Mode_Zone = JFIX(getParameterValue(42+N_Layers_HX+4*N_Layers_Soil)+0.5)  !1=Energy Balance, 2=Kusuda, 3=Input Value
    Mode_Field = JFIX(getParameterValue(43+N_Layers_HX+4*N_Layers_Soil)+0.5) !1=Energy Balance, 2=Kusuda, 3=Input Value
    LU_OutputFile = JFIX(getParameterValue(44+N_Layers_HX+4*N_Layers_Soil)+0.5)
    Tolerance = getParameterValue(45+N_Layers_HX+4*N_Layers_Soil)
    Emissivity_Soil = getParameterValue(46+N_Layers_HX+4*N_Layers_Soil)
    Absorptance_Soil = getParameterValue(47+N_Layers_HX+4*N_Layers_Soil)
    Emissivity_Insul = getParameterValue(48+N_Layers_HX+4*N_Layers_Soil)
    Absorptance_Insul = getParameterValue(49+N_Layers_HX+4*N_Layers_Soil)
    Do n=1,N_Pipes
       Inlet(n) = JFIX(getParameterValue(50+N_Layers_HX+4*N_Layers_Soil+2*(n-1))+0.5)        !0=Use input value, n=Use that pipe's outlet
       IDirection(n) = JFIX(getParameterValue(51+N_Layers_HX+4*N_Layers_Soil+2*(N-1))+0.5)   !1=Standard direction, 2=Reverse direction
    EndDo

   !Calculate the number of inlets and outlets
    N_Inlets = 0
    Do n = 1,N_Pipes
       If (Inlet(n) == 0) N_Inlets = N_Inlets+1
    EndDo

    X_Outlet = 1.
    N_Outlets = 0
    Do n = 1,N_Pipes
       Do nn = 1,N_Pipes
          If (n == Inlet(nn)) Then
             N_Outlets = N_Outlets+1
             X_Outlet(n) = 0.
          EndIf
       EndDo
    EndDo
    N_Outlets = N_Pipes-N_Outlets

   !Order the pipes for the best calling Order
    I_Found = 0
    Do I = 1,N_Pipes
       If (Inlet(I) == 0) Then
          Order(I_Found+1) = I
          I_Found = I_Found+1
          15 J_Found = 0
          Do J = 1,N_Pipes
             If (Inlet(J) == Order(I_Found)) Then
                J_Found = J_Found+1
                Order(I_Found+1) = J
                I_Found = I_Found+1
                Exit
             EndIf
          EndDo
          If (J_Found > 0) Goto 15
       EndIf
    EndDo

 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Values of the Inputs to the Model at the Current Iteration
 T_Fluid_in = getInputValue(1)
 Flow_in = getInputValue(2)/DBLE(N_Inlets)
 T_Zone = getInputValue(3)
 Hconv_Zone = getInputValue(4)
 Trad_Zone = getInputValue(5)
 Qrad_Zone = getInputValue(6)
 Tsurface_Zone = getInputValue(7)
 T_Ambient = getInputValue(8)
 Hconv_Ambient = getInputValue(9)
 T_Sky = getInputValue(10)
 Q_Solar = getInputValue(11)
 Tsurface_Soil = getInputValue(12)
 Velocity_Wash_x = getInputValue(13)   !m/s
 Velocity_Wash_y = getInputValue(14)   !m/s

!Check the inputs for problems
 If (Flow_in < 0.) Call FoundBadInput(2,'Fatal','The flow rate is negative.')
 If (Hconv_Zone < 0.) Call FoundBadInput(4,'Fatal','The zone convection coefficient is negative.')
 If (Qrad_Zone < 0.) Call FoundBadInput(6,'Fatal','The zone incident short-wave radiation is negative.')
 If (Hconv_Ambient < 0.) Call FoundBadInput(9,'Fatal','The ambient convection coefficient is negative.')
 If (Q_Solar < 0.) Call FoundBadInput(11,'Fatal','The incident solar radiation is negative.')
 If (Velocity_Wash_x < 0.) Call FoundBadInput(13,'Fatal','The axial groundwater flow velocity is negative.')
 If (Velocity_Wash_y < 0.) Call FoundBadInput(14,'Fatal','The transverse groundwater flow velocity is negative.')
 If (ErrorFound()) Return
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Retrieve the Values from Storage
 N_Items = 16+N_Layers_HX+N_Layers_Soil

 NX_Tot = JFIX(getStaticArrayValue(1)+0.5)
 NY_Tot = JFIX(getStaticArrayValue(2)+0.5)
 NZ_Tot = JFIX(getStaticArrayValue(3)+0.5)
 NX_Beyond_L = JFIX(getStaticArrayValue(4)+0.5)
 NX_Insul_L = JFIX(getStaticArrayValue(5)+0.5)
 NX_Insul_R = JFIX(getStaticArrayValue(6)+0.5)
 NX_Beyond_R = JFIX(getStaticArrayValue(7)+0.5)
 NY_Beyond_B = JFIX(getStaticArrayValue(8)+0.5)
 NY_Insul_B = JFIX(getStaticArrayValue(9)+0.5)
 NY_Between = JFIX(getStaticArrayValue(10)+0.5)
 NY_Insul_F = JFIX(getStaticArrayValue(11)+0.5)
 NY_Beyond_F = JFIX(getStaticArrayValue(12)+0.5)
 NZ_toWash = JFIX(getStaticArrayValue(13)+0.5)
 NZ_Wash = JFIX(getStaticArrayValue(14)+0.5)
 NZ_Insul = JFIX(getStaticArrayValue(15)+0.5)
 NZ_Tot = JFIX(getStaticArrayValue(16)+0.5)

 Do j = 1,N_Layers_HX
    NZ_Pipe(j) = JFIX(getStaticArrayValue(16+j)+0.5)
 EndDo

 Do j = 1,N_Layers_Soil
    NZ_Soil(j) = JFIX(getStaticArrayValue(16+N_Layers_HX+j)+0.5)
 EndDo

 N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+2*NX_Tot*NY_Tot*NZ_Tot+2*N_Fluid*N_Pipes+2*NZ_Tot

 Do i = 1,NX_Tot
    DX(i) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+i)
 EndDo

 Do j = 1,NY_Tot
    DY(j) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+j)
 EndDo

 Do k = 1,NZ_Tot
    DZ(k) = getStaticArrayValue(16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+k)
 EndDo

 N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot

 Do k = 1,NZ_Tot
    Do j = 1,NY_Tot
       Do i = 1,NX_Tot
          Ti_Soil(i,j,k) = getStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i)
          Tf_Soil(i,j,k) = getStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i)
          Tave_Soil(i,j,k) = getStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i)
       EndDo
    EndDo
 EndDo
             
 N_Items = N_Items+2*NX_Tot*NY_Tot*NZ_Tot

 Do n = 1,N_Pipes
    Do m = 1,N_Fluid
       Ti_Fluid(m,n) = getStaticArrayValue(N_Items+(n-1)*N_Fluid+m)
       Tf_Fluid(m,n) = getStaticArrayValue(N_Items+(n-1)*N_Fluid+m)
       Tave_Fluid(m,n) = getStaticArrayValue(N_Items+(n-1)*N_Fluid+m)
    EndDo
 EndDo

 N_Items = N_Items+2*N_Fluid*N_Pipes

 Do k = 1,NZ_Tot
    Ti_Vert(k) = getStaticArrayValue(N_Items+k)
    Tf_Vert(k) = getStaticArrayValue(N_Items+k)
    Tave_Vert(k) = getStaticArrayValue(N_Items+k)
 EndDo

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Perform all of the iterative call calculations here.

!Set the indicator for the nodes which have pipes
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          Have_Pipe(i,j,k) = .False.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+NX_Insul_L+1,NX_Beyond_L+NX_Insul_L+N_Fluid
    Do m = 1,N_Layers_HX
       Do n = 1,N_Pipes_Layer
          NZ_Now = NZ_Pipe(m)
          Do k = NZ_Now,NZ_Now
             j = NY_Beyond_B+NY_Insul_B+n+(n-1)*NY_Between
             Have_Pipe(i,j,k) = .True.
          EndDo
       EndDo
    EndDo
 EndDo

!Set the indicator for which nodes have vertical surface insulation
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          Have_FrontInsul(i,j,k) = .False.
          Have_BackInsul(i,j,k) = .False.
          Have_LeftInsul(i,j,k) = .False.
          Have_RightInsul(i,j,k) = .False.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B,NY_Beyond_B
       Do k = 1,NZ_Insul
          Have_FrontInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Tot-NY_Beyond_F-1,NY_Tot-NY_Beyond_F-1
       Do k = 1,NZ_Insul
          Have_FrontInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B+1,NY_Beyond_B+1
       Do k = 1,NZ_Insul
          Have_BackInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Tot-NY_Beyond_F,NY_Tot-NY_Beyond_F
       Do k = 1,NZ_Insul
          Have_BackInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L,NX_Beyond_L
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Do k = 1,NZ_Insul
          Have_RightInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Tot-NX_Beyond_R-1,NX_Tot-NX_Beyond_R-1
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Do k = 1,NZ_Insul
          Have_RightInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Beyond_L+1,NX_Beyond_L+1
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Do k = 1,NZ_Insul
          Have_LeftInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

 Do i = NX_Tot-NX_Beyond_R,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Do k = 1,NZ_Insul
          Have_LeftInsul(i,j,k) = .True.
       EndDo
    EndDo
 EndDo

!Calculate the Reynolds and Prandtl numbers of the fluid
 Reynolds = 4.*Flow_In/Pi/D_Pipe_i/Mu_Fluid
 Prandtl = Cp_Fluid*Mu_Fluid/K_Fluid

!Calculate the internal heat transfer coefficient and thermal resistance
 If (Reynolds <= 0.) Then
    Nusselt = 1.
 ElseIf (Reynolds <= 2300.) Then
    Nusselt = 3.66   
 Else
    Nusselt = 0.023*(Reynolds**0.8)*(Prandtl**(1./3.))
 EndIf

 H_Inner = Nusselt*K_Fluid/D_Pipe_i
 SA_Inner = Pi*D_Pipe_i*L_Pipe/DBLE(N_Fluid)

 If (H_Inner <= 0.) Then
    Resistance_Fluid = 9.9999d+20
 Else
    Resistance_Fluid = 1./H_Inner/SA_Inner
 EndIf

!Calculate the wall resistance
 Resistance_Wall = DLOG(D_Pipe_O/D_Pipe_i)/2./Pi/(L_Pipe/DBLE(N_Fluid))/K_Pipe + R_Contact 

!Calculate the volume of the nodes that contain a pipe
 Volume_PipeNode = (Radial_Factor*D_Pipe_o)*(Radial_Factor*D_Pipe_o)*(L_Pipe/DBLE(N_Fluid))
 Volume_PipeNode = Volume_PipeNode-Pi*D_Pipe_o*D_Pipe_o/4.*L_Pipe/DBLE(N_Fluid)

!Calculate the effective diameter of the soil that gives the same volume
 Dia_PipeNode = ((Volume_PipeNode*4./Pi/(L_Pipe/DBLE(N_Fluid))+D_Pipe_o*D_Pipe_o)**0.5)

!Calculate the resistance to the adjacent soil node (radial direction) using radial heat transfer
 Radius_Node = D_Pipe_O/2.+(Dia_PipeNode/2.-D_Pipe_O/2.)/2.
 Radius_Inner = D_Pipe_O/2.
 Radius_Outer = Dia_PipeNode/2.

 Do j = 1,N_Layers_HX
    k = NZ_Pipe(j)
    I_Soil = 1
    Do i = 1,N_Layers_Soil
       If (k > NZ_Soil(i)) Then
          I_Soil = I_Soil+1
       EndIf
    EndDo
    Resistance_Radial_i = DLOG(Radius_Node/Radius_Inner)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil)  
    Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the area for each node

   !Calculate the overall heat transfer coefficient to the adjacent soil nodes from the fluid
    UA_Radial(j) = 1./(Resistance_Fluid+Resistance_Wall+Resistance_Radial_i)*Length_Factor
 EndDo

!Calculate the mass of fluid in each node
 Mass_FluidNode = Pi*D_Pipe_i*D_Pipe_i/4.*L_Pipe/DBLE(N_Fluid)*Rho_Fluid

!Set a few soil properties
 Time_Year = TIME/24.
 Time_Year = DMOD(Time_Year,365.)

!Start the nodal calculations here
 Iters = 0
 200 Continue
 Q_Env = 0.
 Q1_Tot = 0.
 Q2_Tot = 0.
 Q3_Tot = 0.

!Step along each pipe - calling the best pipe first
 Do p = 1,N_Pipes

   !Find the pipe to call 
    n = Order(p)
    I_Layer = INT((n-1)/N_Pipes_Layer)+1   !Layer in which pipe is located
    J_Layer = n-(I_Layer-1)*N_Pipes_Layer  !Pipe number in current layer (5 of 7 for example)

   !Step along each pipe
    If (IDirection(n) == 1) Then
       I_Start = 1
       I_Stop = N_Fluid
       I_Step = 1
    Else
       I_Start = N_Fluid
       I_Stop = 1
       I_Step = -1
    EndIf
    
    Do i = I_Start,I_Stop,I_Step

      !Figure out which soil node contains this pipe node
       I_Node = NX_Beyond_L+NX_Insul_L+i
       K_Node = NZ_Pipe(I_Layer)
       J_Node = NY_Beyond_B+NY_Insul_B+J_Layer+(J_Layer-1)*NY_Between

      !Set the inlet fluid temperature to the node
       If ((IDirection(n) == 1).AND.(i == 1).AND.(Inlet(n) == 0)) Then              !Inlet node for inlet pipe - standard
          T_In_Node = T_Fluid_In

       ElseIf ((IDirection(n) == 2).AND.(i == N_Fluid).AND.(Inlet(n) == 0)) Then   !Inlet node for inlet pipe - reverse
          T_In_Node = T_Fluid_In

       ElseIf ((IDirection(n) == 1).AND.(i == 1)) Then                           !Inlet node for a conected pipe - standard
          nn = Inlet(n)
          If (IDirection(nn) == 1) Then
             T_In_Node = Tave_Fluid(N_Fluid,nn)
          Else
             T_In_Node = Tave_Fluid(1,nn)
          EndIf
          
       ElseIf ((IDirection(n) == 2).AND.(i == N_Fluid)) Then                 !Inlet node for a conected pipe - reverse
          nn = Inlet(n)
          If (IDirection(nn) == 1) Then
             T_In_Node = Tave_Fluid(N_Fluid,nn)
          Else
             T_In_Node = Tave_Fluid(1,nn)
          EndIf

       ElseIf (IDirection(n) == 1) Then                                      !Standard node for a conected pipe - standard
          T_In_Node = Tave_Fluid(i-1,n)

       ElseIf (IDirection(n) == 2) Then                                      !Standard node for a conected pipe - reverse
          T_In_Node = Tave_Fluid(i+1,N)

       Else
          Call Messages(-1,MessageL,'Fatal',CurrentUnit,CurrentType)
       EndIf

      !Step through each fluid node and put the differential equation in the form dT/dt=aT+b
       AA_Fluid = (-Flow_In*Cp_Fluid)/Mass_FluidNode/Cp_Fluid
       BB_Fluid = (Flow_In*Cp_Fluid*T_In_Node)/Mass_FluidNode/Cp_Fluid

      !Account for the radial heat transfer from the pipes to the adjacent soil node
       AA_Fluid = AA_Fluid-UA_Radial(I_Layer)/Mass_FluidNode/Cp_Fluid
       BB_Fluid = BB_Fluid+(UA_Radial(I_Layer)*Tave_Soil(I_Node,J_Node,K_Node))/Mass_FluidNode/Cp_Fluid

      !Calculate the fluid temperatures
       If (AA_Fluid == 0.) Then
          Tf_Fluid(i,n) = Ti_Fluid(i,n)+BB_Fluid*Timestep
          Tave_Fluid(i,n) = Ti_Fluid(i,n)+BB_Fluid*Timestep/2.
       Else
          Tf_Fluid(i,n) = (Ti_Fluid(i,n)+BB_Fluid/AA_Fluid)*DEXP(AA_Fluid*Timestep)-BB_Fluid/AA_Fluid
          Tave_Fluid(i,n) = (Ti_Fluid(i,n)+BB_Fluid/AA_Fluid)*(DEXP(AA_Fluid*Timestep)-1.)/AA_Fluid/Timestep-BB_Fluid/AA_Fluid
       EndIf

      !Calculate the heat loss
       Q_PipetoSoil(n,i) = UA_Radial(I_Layer)*(Tave_Fluid(i,n)-Tave_Soil(I_Node,J_Node,K_Node))
       Q_Env(n) = Q_Env(n)+UA_Radial(I_Layer)*(Tave_Fluid(i,n)-Tave_Soil(I_Node,J_Node,K_Node))

       Q1 = UA_Radial(I_Layer)*(Tave_Fluid(i,n)-Tave_Soil(I_Node,J_Node,K_Node))
       Q2 = Flow_In*Cp_Fluid*(T_In_Node-Tave_Fluid(i,n))
       Q3 = Mass_FluidNode*Cp_Fluid*(Tf_Fluid(i,n)-Ti_Fluid(i,n))/Timestep

       Q1_Tot = Q1_Tot+Q1
       Q2_Tot = Q2_Tot+Q2
       Q3_Tot = Q3_Tot+Q3
       
    EndDo

 EndDo

!Calculate the leaving fluid temperature
 T_Fluid_Out = 0.
 Do n = 1,N_Pipes
    If (IDirection(n) == 1) Then
       T_Fluid_Out = T_Fluid_Out+Tave_Fluid(N_Fluid,n)*X_Outlet(n)
    Else
       T_Fluid_Out = T_Fluid_Out+Tave_Fluid(1,n)*X_Outlet(n)
    EndIf
 EndDo
 T_Fluid_Out = T_Fluid_Out/DBLE(N_Outlets)

!Calculate the surface temperatures above the near field
 Tsurf_Kusuda = T_Ave_Soil-T_Amp_Soil*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil))

!Initialize a few surface energy balance terms
 Qconvection_near = 0.
 Qshortwave_near = 0.
 Qlongwave_near = 0.
 Qconduction_near = 0.
 Qconvection_zone = 0.
 Qshortwave_zone = 0.
 Qlongwave_zone = 0.
 Qconduction_zone = 0.

 Do i=1,NX_Tot
    Do j=1,NY_Tot
   
       If (Mode_Field == 1) Then
          Iters_Surface = 0
          Tsurface_Previous = 10.

          20 Hrad_Soil = h_Radiation(T_Sky,Tsurface_Previous,Emissivity_Soil)

          Tsurf_o(i,j) = (Hconv_Ambient*T_Ambient+Hrad_Soil*T_Sky+Q_Solar*Absorptance_Soil+2.*K_Soil(1)*Tave_Soil(i,j,1)/DZ(1))/(Hconv_Ambient+Hrad_Soil+2.*K_Soil(1)/DZ(1))

          If (Dabs(Tsurf_o(i,j)-Tsurface_Previous) > Tolerance) Then
             Iters_Surface = Iters_Surface+1
             If (Iters_Surface < 100) Then
                Tsurface_Previous = Tsurf_o(i,j)
                GoTo 20
             EndIf
          EndIf

          If ((i <= NX_Beyond_L).or.(i > NX_Tot-NX_Beyond_R).or.(j <= NY_Beyond_B).or.(j > NY_Tot-NY_Beyond_F)) Then
             Qconvection_near = Qconvection_near+Hconv_Ambient*DX(i)*DY(j)*(Tsurf_o(i,j)-T_Ambient)
             Qshortwave_near = Qshortwave_near+Q_Solar*Absorptance_Soil*DX(i)*DY(j)
             Qlongwave_near = Qlongwave_near+Hrad_soil*DX(i)*DY(j)*(Tsurf_o(i,j)-T_Sky)
             Qconduction_near = Qconduction_near+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
          Else
             Qconvection_near = Qconvection_near
             Qshortwave_near = Qshortwave_near
             Qlongwave_near = Qlongwave_near
             Qconduction_near = Qconduction_near
          EndIf

       ElseIf (Mode_Field == 2) Then
          Tsurf_o(i,j) = Tsurf_Kusuda
          If ((i <= NX_Beyond_L).or.(i > NX_Tot-NX_Beyond_R).or.(j <= NY_Beyond_B).or.(j > NY_Tot-NY_Beyond_F)) Then
             Qconduction_near = Qconduction_near+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
          Else
             Qconduction_near = Qconduction_near
          EndIf
       Else
          Tsurf_o(i,j) = Tsurface_Soil
          If((i <= NX_Beyond_L).or.(i > NX_Tot-NX_Beyond_R).or.(j <= NY_Beyond_B).or.(j > NY_Tot-NY_Beyond_F)) Then
             Qconduction_near = Qconduction_near+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
          Else
             Qconduction_near = Qconduction_near
          EndIf
       EndIf	 

    EndDo
 EndDo

!Calculate the surface temperatures above the heat exchanger
 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
   
       If (Mode_Zone == 1) Then
          Iters_Surface = 0
          Tsurface_Previous = 10.
          30 Hrad_Zone = h_Radiation(Trad_Zone,Tsurface_Previous,Emissivity_Insul)
          R_Conduction = DZ(1)/2./K_Soil(1)+R_Cover

          Tsurf_i(i,j) = (R_Conduction*Hconv_Zone*T_Zone+R_Conduction*Hrad_Zone*Trad_Zone+R_Conduction*Qrad_Zone*Absorptance_Insul+Tave_Soil(i,j,1))/(1.+R_Conduction*Hrad_Zone+R_Conduction*Hconv_Zone)
       
          Q_temp = (Tsurf_i(i,j)-Tave_Soil(i,j,1))/R_Conduction
       
          If (R_Cover <= 0.) Then
             Tsurf_o(i,j)=Tsurf_i(i,j)   !i=Closest to zone
          Else
             Tsurf_o(i,j)=Tsurf_i(i,j)-Q_temp*R_Cover
          EndIf

          If (Dabs(Tsurf_i(i,j)-Tsurface_Previous) > Tolerance) Then
             Iters_Surface=Iters_Surface+1
             If (Iters_Surface < 100) Then
                Tsurface_Previous=Tsurf_i(i,j)
                GoTo 30
             EndIf
          EndIf

          Qconvection_zone = Qconvection_zone+Hconv_Zone*DX(i)*DY(j)*(Tsurf_i(i,j)-T_Zone)
          Qshortwave_zone = Qshortwave_zone+Qrad_Zone*Absorptance_Insul*DX(i)*DY(j)
          Qlongwave_zone = Qlongwave_zone+Hrad_Zone*DX(i)*DY(j)*(Tsurf_i(i,j)-Trad_Zone)
          Qconduction_zone = Qconduction_zone+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)

       ElseIf (Mode_Zone == 2) Then
          Tsurf_o(i,j) = Tsurf_Kusuda
          Tsurf_i(i,j) = Tsurf_Kusuda
          Qconduction_zone = Qconduction_zone+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
       Else
          Tsurf_i(i,j) = Tsurface_Zone
          If (R_Cover <= 0.) Then
             Tsurf_o(i,j) = Tsurf_i(i,j)   !i=Closest to zone
             Qconduction_zone = Qconduction_zone+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
          Else
             R_Surface = DZ(1)/2./k_Soil(1)/DX(i)/DY(j)+R_Cover/DX(i)/DY(j)
             Q_temp = (Tsurf_i(i,j)-Tave_Soil(i,j,1))/R_Surface
             Tsurf_o(i,j) = Tsurf_i(i,j)-Q_temp*R_Cover/DX(i)/DY(j)
             Qconduction_zone = Qconduction_zone+2.*K_Soil(1)*(Tsurf_o(i,j)-Tave_Soil(i,j,1))/DZ(1)*DX(i)*DY(j)
          EndIf
       EndIf	 

    EndDo
 EndDo

!Calculate the deep earth temperature profile
 Alpha_Soil = K_Soil(N_Layers_Soil)/Rho_Soil(N_Layers_Soil)/CP_Soil(N_Layers_Soil)*24.   !m2/day
 Z_DeepEarth = Depth_Pipe(N_Layers_HX)+Radial_Factor*D_Pipe_o/2.+D_DeepEarth
 T_Deep = T_Ave_Soil-T_Amp_Soil*DEXP(-Z_DeepEarth*((Pi/365./Alpha_Soil)**0.5))*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil-Z_DeepEarth/2.*((365./Pi/Alpha_Soil)**0.5)))
 
!Calculate the undisturbed soil temperature profile
 If (Mode_Field == 2) Then
    Depth_Now = 0.
    I_Soil = 1
    Do k = 1,NZ_Tot
       If (k > NZ_Soil(I_Soil)) I_Soil=I_Soil+1
       Alpha_Soil = K_Soil(I_Soil)/Rho_Soil(I_Soil)/CP_Soil(I_Soil)*24.   !m2/day
       Depth = Depth_Now+DZ(k)/2.
       Tave_Vert(k) = T_Ave_Soil-T_Amp_Soil*DEXP(-Depth*((Pi/365./Alpha_Soil)**0.5))*DCOS(2.*Pi/365.*(Time_Year-TimeShift_Soil-Depth/2.*((365./Pi/Alpha_Soil)**0.5)))
       Depth_Now = Depth_Now+DZ(k)
    EndDo
 Else
    Iters_Vert = 0
    35 Continue

    If (Mode_Field == 1) Then

       Iters_Surface = 0
       Tsurface_Previous = 10.

       40 Hrad_Soil = h_Radiation(T_Sky,Tsurface_Previous,Emissivity_Soil)

       Tsurf_Farfield = (Hconv_Ambient*T_Ambient+Hrad_Soil*T_Sky+Q_Solar*Absorptance_Soil+2.*K_Soil(1)*Tave_Vert(1)/DZ(1))/(Hconv_Ambient+Hrad_Soil+2.*K_Soil(1)/DZ(1))

       If (Dabs(Tsurf_Farfield-Tsurface_Previous) > Tolerance) Then
          Iters_Surface = Iters_Surface+1
          If (Iters_Surface < 100) Then
             Tsurface_Previous = Tsurf_Farfield
             GoTo 40
          EndIf
       EndIf
    Else
       Tsurf_Farfield = Tsurface_Soil
    EndIf	 
        
    I_Soil = 1
    Do k = 1,NZ_Tot
       If (k > NZ_Soil(I_Soil)) I_Soil=I_Soil+1
       Cap_Node = 1.*1.*DZ(k)*Rho_Soil(I_Soil)*CP_Soil(I_Soil)
       AA_Soil = 0.
       BB_Soil = 0.

       If (k == 1) Then
          R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/1./1.
          R_Cond_2 = 0.
          R_Cond_T = R_Cond_1+R_Cond_2
          UA_Cond_T = 1./R_Cond_T
          T_Cond = Tsurf_Farfield

          AA_Soil = AA_Soil - UA_Cond_T/Cap_Node
          BB_Soil = BB_Soil + UA_Cond_T*T_Cond/Cap_Node
       Else 
          R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/1./1.
          R_Cond_2 = DZ(k-1)/2./K_Soil(I_Soil)/1./1.
          R_Cond_T = R_Cond_1+R_Cond_2
          UA_Cond_T = 1./R_Cond_T
          T_Cond = Tave_Vert(k-1)

          AA_Soil = AA_Soil - UA_Cond_T/Cap_Node
          BB_Soil = BB_Soil + UA_Cond_T*T_Cond/Cap_Node
       EndIf

       If (k == NZ_Tot) Then
          R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/1./1.
          R_Cond_2 = 0.
          R_Cond_T = R_Cond_1+R_Cond_2
          UA_Cond_T = 1./R_Cond_T
          T_Cond = T_Deep

          AA_Soil = AA_Soil - UA_Cond_T/Cap_Node
          BB_Soil = BB_Soil + UA_Cond_T*T_Cond/Cap_Node
       Else 
          R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/1./1.
          R_Cond_2 = DZ(k+1)/2./K_Soil(I_Soil)/1./1.
          R_Cond_T = R_Cond_1+R_Cond_2
          UA_Cond_T = 1./R_Cond_T
          T_Cond = Tave_Vert(k+1)

          AA_Soil = AA_Soil - UA_Cond_T/Cap_Node
          BB_Soil = BB_Soil + UA_Cond_T*T_Cond/Cap_Node
       EndIf

       If (AA_Soil == 0.) Then
          Tf_Vert(k) = Ti_Vert(k)+BB_Soil*Timestep
          Tnew_Vert(k) = Ti_Vert(k)+BB_Soil*Timestep/2.
       Else
          Tf_Vert(k) = (Ti_Vert(k)+BB_Soil/AA_Soil)*DEXP(AA_Soil*Timestep)-BB_Soil/AA_Soil
          Tnew_Vert(k) = (Ti_Vert(k)+BB_Soil/AA_Soil)*(DEXP(AA_Soil*Timestep)-1.)/AA_Soil/Timestep-BB_Soil/AA_Soil
       EndIf

    EndDo

    Converged = .True.
    Do k = 1,NZ_Tot
       If ((DABS(Tnew_Vert(k)-Tave_Vert(k)) > Tolerance).and.(Iters_Vert <= Max_Iters)) Then
          Converged = .False.
       EndIf
    EndDo

    If (Converged) Then
       Do k = 1,NZ_Tot
          Tave_Vert(k) = Tnew_Vert(k)
       EndDo
    Else
       Iters_Vert = Iters_Vert+1
       Do k = 1,NZ_Tot
          Tave_Vert(k) = Tnew_Vert(k)
       EndDo
       GoTo 35
    EndIf

 EndIf

!Calculate the soil temperatures
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot

   !Set the properties of the soil nodes
          If (k == 1) Then
             I_Soil = 1
          ElseIf (k > NZ_Soil(I_Soil)) Then
             I_Soil = I_Soil+1
          EndIf

   !Calculate the conductivity of the soil above and below this node
          K_Above = K_Soil(I_Soil)
          If (k == 1) Then
             K_Above = K_Soil(1)
          Else
             Do m = 1,N_Layers_Soil
                If ((k-1) <= NZ_Soil(m)) Then
                   K_Above=K_Soil(m)
                   Exit
                EndIf
             EndDo
          EndIf

          K_Below = K_Soil(I_Soil)
          If (k == NZ_Tot) Then
             K_Below = K_Soil(N_Layers_Soil)
          Else
             Do m = 1,N_Layers_Soil
                If ((k+1) > NZ_Soil(m)) Then
                   K_Below = K_Soil(m+1)
                EndIf
             EndDo
          EndIf

   !Set the capacitance of each node 
          If (Have_Pipe(i,j,k)) Then
             Cap_Soil(i,j,k) = DX(i)*DY(j)*DZ(k)*Rho_Soil(I_Soil)*CP_Soil(I_Soil)-Rho_Soil(I_Soil)*CP_Soil(I_Soil)*Pi*D_Pipe_o*D_Pipe_o/4.*DX(i)
          Else
             Cap_Soil(i,j,k) = DX(i)*DY(j)*DZ(k)*Rho_Soil(I_Soil)*CP_Soil(I_Soil)
          EndIf

   !Initialize the a and b terms for the differential equation
          AA_Soil = 0.
          BB_Soil = 0.

   !Front surface heat transfer
          If (Have_FrontInsul(i,j,k)) Then
             R_Cond_3 = R_Vertical/DX(i)/DZ(k)
          Else
             R_Cond_3 = 0.
          EndIf

          If ((j == NY_Tot).and.(Have_Pipe(i,j,k))) Then
             If (BoundaryMode_Front == 1) Then
                Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
                R_Cond_1 = Resistance_Radial_o
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond=Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (j == NY_Tot) Then
             If (BoundaryMode_Front == 1) Then
                R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i,j+1,k)).and.(Have_Pipe(i,j,k))) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j+1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = DY(j+1)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j+1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j+1,k)) Then
             R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j+1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_2 = DY(j+1)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j+1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Front surface heat transfer
          Qfront(i,j,k) = UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Back surface heat transfer
          If (Have_BackInsul(i,j,k)) Then
             R_Cond_3 = R_Vertical/DX(i)/DZ(k)
          Else
             R_Cond_3 = 0.
          EndIf

          If ((j == 1).and.(Have_Pipe(i,j,k))) Then
             If (BoundaryMode_Back == 1) Then
                Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
                R_Cond_1 = Resistance_Radial_o
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (j == 1) Then
             If (BoundaryMode_Back == 1) Then
                R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i,j-1,k)).and.(Have_Pipe(i,j,k))) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j-1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = DY(j-1)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j-1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j-1,k)) Then
             R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j-1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DY(j)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_2 = DY(j-1)/2./K_Soil(I_Soil)/DX(i)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j-1,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Back surface heat transfer
          Qback(i,j,k) = UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Left surface heat transfer
          If (Have_LeftInsul(i,j,k)) Then
             R_Cond_3 = R_Vertical/DY(j)/DZ(k)
          Else
             R_Cond_3 = 0.
          EndIf

          If ((i == 1).and.(Have_Pipe(i,j,k))) Then
             If (BoundaryMode_Left == 1) Then
                A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
                R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (i == 1) Then
             If (BoundaryMode_Left == 1) Then
                R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i-1,j,k)).and.(Have_Pipe(i,j,k))) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i-1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i-1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i-1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i-1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i-1,j,k)) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i-1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i-1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             R_Cond_2 = DX(i-1)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i-1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Left surface heat transfer
          Qleft(i,j,k) = UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Right surface heat transfer
          If (Have_RightInsul(i,j,k)) Then
             R_Cond_3 = R_Vertical/DY(j)/DZ(k)
          Else
             R_Cond_3 = 0.
          EndIf

          If ((i == NX_Tot).and.(Have_Pipe(i,j,k))) Then
             If (BoundaryMode_Right == 1) Then
                A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
                R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (i == NX_Tot) Then
             If (BoundaryMode_Right == 1) Then
                R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Vert(k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i+1,j,k)).and.(Have_Pipe(i,j,k))) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i+1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i+1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i+1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i+1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i+1,j,k)) Then
             A_Cond = DY(j)*DZ(k)-Pi*D_Pipe_o*D_Pipe_o/4.
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_2 = DX(i+1)/2./K_Soil(I_Soil)/A_Cond
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i+1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DX(i)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             R_Cond_2 = DX(i+1)/2./K_Soil(I_Soil)/DY(j)/DZ(k)
             R_Cond_T = R_Cond_1+R_Cond_2+R_Cond_3
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i+1,j,k)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Right surface heat transfer
          Qright(i,j,k) = UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Top surface heat transfer
          If ((k == 1).and.(Have_Pipe(i,j,k))) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tsurf_o(i,j)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (k == 1) Then
             R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tsurf_o(i,j)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i,j,k-1)).and.(Have_Pipe(i,j,k))) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Above)   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k-1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = DZ(k-1)/2./K_Above/DX(i)/DY(j)
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k-1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k-1)) Then
             R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Above)   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k-1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             R_Cond_2 = DZ(k-1)/2./K_Above/DX(i)/DY(j)
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k-1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Top surface heat transfer
          Qtop(i,j,k) = UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Bottom surface heat transfer
          If ((k == NZ_Tot).and.(Have_Pipe(i,j,k))) Then
             If (BoundaryMode_Bottom == 1) Then
                Resistance_Radial_o=4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
                R_Cond_1 = Resistance_Radial_o
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = T_Deep

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (k == NZ_Tot) Then
             If (BoundaryMode_Bottom == 1) Then
                R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             Else
                R_Cond_1 = 1.d+20
             EndIf
             R_Cond_2 = 0.
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = T_Deep

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf ((Have_Pipe(i,j,k+1)).and.(Have_Pipe(i,j,k))) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Below)   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k+1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k)) Then
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Soil(I_Soil))   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_1 = Resistance_Radial_o
             R_Cond_2 = DZ(k+1)/2./K_Below/DX(i)/DY(j)
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k+1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          ElseIf (Have_Pipe(i,j,k+1)) Then
             R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             Resistance_Radial_o = 4.*(DLOG(Radius_Outer/Radius_Node)/2./PI/(L_Pipe/DBLE(N_Fluid))/K_Below)   !Factor of 4 to account for 1/4 of the are for each node
             R_Cond_2 = Resistance_Radial_o
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k+1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          Else 
             R_Cond_1 = DZ(k)/2./K_Soil(I_Soil)/DX(i)/DY(j)
             R_Cond_2 = DZ(k+1)/2./K_Below/DX(i)/DY(j)
             R_Cond_T = R_Cond_1+R_Cond_2
             UA_Cond_T = 1./R_Cond_T
             T_Cond = Tave_Soil(i,j,k+1)

             AA_Soil = AA_Soil - UA_Cond_T/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Cond_T/Cap_Soil(i,j,k)*T_Cond

          EndIf

!   Bottom surface heat transfer
          Qbottom(i,j,k )= UA_Cond_T*(Tave_Soil(i,j,k)-T_Cond)

   !Heat transfer with the pipe
          If (Have_Pipe(i,j,k)) Then
      
      !Figure out which pipe node is in this soil node
             I_Node = i-NX_Beyond_L-NX_Insul_L
             J_Pipe = (j+NY_Between-NY_Beyond_B-NY_Insul_B)/(1+NY_Between)

             Do n = 1,N_Layers_HX
                If (k == NZ_Pipe(n)) Then
                   K_Layer = n
                   Exit
                EndIf
             EndDo
             I_Pipe = (K_Layer-1)*N_Pipes_Layer+J_Pipe

             AA_Soil = AA_Soil - UA_Radial(K_Layer)/Cap_Soil(i,j,k)
             BB_Soil = BB_Soil + UA_Radial(K_Layer)/Cap_Soil(i,j,k)*Tave_Fluid(I_Node,I_Pipe)

!      Pipe heat transfer
             Qpipe(i,j,k) = UA_Radial(K_Layer)*(Tave_Soil(i,j,k)-Tave_Fluid(I_Node,I_Pipe))

          EndIf

   !Groundwater movement through the wash layer
          If ((k > NZ_toWash).and.(k <= NZ_toWash+NZ_Wash)) Then
             Mdot_Groundwater_x = Velocity_Wash_x*3600.*dy(j)*dz(k)*Rho_Groundwater
             Mdot_Groundwater_y = Velocity_Wash_y*3600.*dx(i)*dz(k)*Rho_Groundwater

             If (i == 1) Then
                AA_Soil = AA_Soil - Mdot_Groundwater_x*Cp_Groundwater/Cap_Soil(i,j,k)
                BB_Soil = BB_Soil + Mdot_Groundwater_x*Cp_Groundwater/Cap_Soil(i,j,k)*Tave_Vert(k)
                Qgroundwater(i,j,k )= Mdot_Groundwater_x*Cp_Groundwater*(Tave_Vert(k)-Tave_Soil(i,j,k))
             Else
                AA_Soil = AA_Soil - Mdot_Groundwater_x*Cp_Groundwater/Cap_Soil(i,j,k)
                BB_Soil = BB_Soil + Mdot_Groundwater_x*Cp_Groundwater/Cap_Soil(i,j,k)*Tave_Soil(i-1,j,k)
                Qgroundwater(i,j,k) = Mdot_Groundwater_x*Cp_Groundwater*(Tave_Soil(i-1,j,k)-Tave_Soil(i,j,k))
             EndIf

             If (j == 1) Then
                AA_Soil = AA_Soil - Mdot_Groundwater_y*Cp_Groundwater/Cap_Soil(i,j,k)
                BB_Soil = BB_Soil + Mdot_Groundwater_y*Cp_Groundwater/Cap_Soil(i,j,k)*Tave_Vert(k)
                Qgroundwater(i,j,k) = Qgroundwater(i,j,k)+Mdot_Groundwater_x*Cp_Groundwater*(Tave_Vert(k)-Tave_Soil(i,j,k))
             Else
                AA_Soil = AA_Soil - Mdot_Groundwater_y*Cp_Groundwater/Cap_Soil(i,j,k)
                BB_Soil = BB_Soil + Mdot_Groundwater_y*Cp_Groundwater/Cap_Soil(i,j,k)*Tave_Soil(i,j-1,k)
                Qgroundwater(i,j,k) = Qgroundwater(i,j,k)+Mdot_Groundwater_x*Cp_Groundwater*(Tave_Soil(i,j-1,k)-Tave_Soil(i,j,k))
             EndIf
          Else
             Qgroundwater(i,j,k) = 0.
             Mdot_Groundwater_x = 0.
             Mdot_Groundwater_y = 0.
          EndIf

   !Solve the differential equation analytically
          If (AA_Soil == 0.) Then
             Tf_Soil(i,j,k) = Ti_Soil(i,j,k)+BB_Soil*Timestep
             Tnew_Soil(i,j,k) = Ti_Soil(i,j,k)+BB_Soil*Timestep/2.
          Else
             Tf_Soil(i,j,k) = (Ti_Soil(i,j,k)+BB_Soil/AA_Soil)*DEXP(AA_Soil*Timestep)-BB_Soil/AA_Soil
             Tnew_Soil(i,j,k) = (Ti_Soil(i,j,k)+BB_Soil/AA_Soil)*(DEXP(AA_Soil*Timestep)-1.)/AA_Soil/Timestep-BB_Soil/AA_Soil
          EndIf

   !Stored heat transfer
          Qstore(i,j,k) = Cap_Soil(i,j,k)*(Tf_Soil(i,j,k)-Ti_Soil(i,j,k))/Timestep

   !Check the energy balance
          Balance(i,j,k) = Qstore(i,j,k)+Qleft(i,j,k)+Qright(i,j,k)+Qfront(i,j,k)+Qback(i,j,k)+Qtop(i,j,k)+Qbottom(i,j,k)+Qpipe(i,j,k)-Qgroundwater(i,j,k)

       EndDo
    EndDo
 EndDo

!See If the node temperatures have converged
 Converged = .True.
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          If ((DABS(Tnew_Soil(i,j,k)-Tave_Soil(i,j,k)) > Tolerance).and.(Iters < Max_Iters)) Then
             Converged = .False.
          EndIf
       EndDo
    EndDo
 EndDo

 If (Converged) Then
    Do i = 1,NX_Tot
       Do j = 1,NY_Tot
          Do k = 1,NZ_Tot
             Tave_Soil(i,j,k) = Tnew_Soil(i,j,k)
          EndDo
       EndDo
    EndDo
 Else
    Iters = Iters+1
    Do i = 1,NX_Tot
       Do j = 1,NY_Tot
          Do k = 1,NZ_Tot
             Tave_Soil(i,j,k) = Tnew_Soil(i,j,k)
          EndDo
       EndDo
    EndDo
    Goto 200
 EndIf

!Warn the user if convergence wasn't reached
 If(Iters==Max_Iters) Then
    Call Messages(-1,NonConvWarning,'Warning',CurrentUnit,CurrentType)
 EndIf

!Check the nodal balances and provide a breakpoint for debugging           
! Do i = 1,NX_Tot
!    Do j = 1,NY_Tot
!       Do k = 1,NZ_Tot
!          If (DABS(Balance(i,j,k)) > 0.01) Then
!             Balance(i,j,k) = Balance(i,j,k)
!          EndIf
!       EndDo
!    EndDo
! EndDo

 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          If (Have_Pipe(i,j,k)) Then
             I_Node = i-NX_Beyond_L-NX_Insul_L
             J_Pipe = (j+NY_Between-NY_Beyond_B-NY_Insul_B)/(1+NY_Between)
             Do n = 1,N_Layers_HX
                If (k == NZ_Pipe(n)) Then
                   K_Layer = n
                   Exit
                EndIf
             EndDo
             I_Pipe = (K_Layer-1)*N_Pipes_Layer+J_Pipe
             Balance_Pipe = DABS(Qpipe(i,j,k)+Q_PipetoSoil(I_Pipe,I_Node))

!             If (DABS(Balance_Pipe) > 0.01) Then
!                Balance_Pipe = Balance_Pipe
!             EndIf
          EndIf
       EndDo
    EndDo
 EndDo

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Temporary Values in Storage
 N_Items = 16+N_Layers_HX+N_Layers_Soil+NX_Tot+NY_Tot+NZ_Tot+NX_Tot*NY_Tot*NZ_Tot

 Do k = 1,NZ_Tot
    Do j = 1,NY_Tot
       Do i = 1,NX_Tot
          Call SetStaticArrayValue(N_Items+(k-1)*NX_Tot*NY_Tot+(j-1)*NX_Tot+i,Tf_Soil(i,j,k))
       EndDo
    EndDo
 EndDo
             
 N_Items = N_Items+NX_Tot*NY_Tot*NZ_Tot

 Do n = 1,N_Pipes
    Do m = 1,N_Fluid
       Call SetStaticArrayValue(N_Items+N_Fluid*N_Pipes+(n-1)*N_Fluid+m,Tf_Fluid(m,n))
    EndDo
 EndDo

 N_Items = N_Items+2*N_Fluid*N_Pipes

 Do k = 1,NZ_Tot
    Call SetStaticArrayValue(N_Items+NZ_Tot+k,Tf_Vert(k))
 EndDo

 N_Items=N_Items+2*NZ_Tot
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Outputs from the Model
 Qstore_T = 0.
 Qstore_f = 0.
 Q_Pipes = 0.
 Q_boundary = 0.
 Q_groundwater_x = 0.
 Q_groundwater_y = 0.
 Q_top = 0.
 Q_top_ins = 0.
 Q_bottom = 0.

!Stored energy in the soil
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          Qstore_T = Qstore_T+Cap_Soil(i,j,k)*(Tf_Soil(i,j,k)-Ti_Soil(i,j,k))/Timestep
       EndDo
    EndDo
 EndDo

!Stored energy in the fluid
 Do n = 1,N_Pipes
    Do m = 1,N_Fluid
       Qstore_f = Qstore_f+Mass_FluidNode*Cp_Fluid*(Tf_Fluid(m,n)-Ti_Fluid(m,n))/Timestep
    EndDo
 EndDo

!Pipe to soil heat transfer
 Do n = 1,N_Pipes
    Q_Pipes = Q_Pipes+Q_Env(n)
 EndDo

!Conductive heat transfer across the far-field boundary
 Do i = 1,NX_Tot
    Do j = 1,1
       Do k = 1,NZ_Tot
          If (k == 1) Then
             I_Soil = 1
          ElseIf (k > NZ_Soil(I_Soil)) Then
             I_Soil = I_Soil+1
          EndIf
       
          If (BoundaryMode_Back == 1) Then
             Q_boundary = Q_boundary+K_Soil(I_Soil)*DX(i)*DZ(k)*2./DY(j)*(Tave_Soil(i,j,k)-Tave_Vert(k))
          Else
             Q_Boundary = Q_Boundary
          EndIf
       EndDo
    EndDo
 EndDo
 
 Do i = 1,NX_Tot
    Do j = NY_Tot,NY_Tot
       Do k = 1,NZ_Tot
          If (k == 1) Then
             I_Soil = 1
          ElseIf (k > NZ_Soil(I_Soil)) Then
             I_Soil = I_Soil+1
          EndIf
       
          If (BoundaryMode_Front == 1) Then
             Q_boundary = Q_boundary+K_Soil(I_Soil)*DX(i)*DZ(k)*2./DY(j)*(Tave_Soil(i,j,k)-Tave_Vert(k))
          Else
             Q_Boundary = Q_Boundary
          EndIf
       EndDo
    EndDo
 EndDo

 Do i = 1,1
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          If (k == 1) Then
             I_Soil = 1
          ElseIf (k > NZ_Soil(I_Soil)) Then
             I_Soil = I_Soil+1
          EndIf
       
          If (BoundaryMode_Left == 1) Then
             Q_boundary = Q_boundary+K_Soil(I_Soil)*DY(j)*DZ(k)*2./DX(i)*(Tave_Soil(i,j,k)-Tave_Vert(k))
          Else
             Q_Boundary = Q_Boundary
          EndIf
       EndDo
    EndDo
 EndDo

 Do i = NX_Tot,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,NZ_Tot
          If (k == 1) Then
             I_Soil = 1
          ElseIf (k > NZ_Soil(I_Soil)) Then
             I_Soil = I_Soil+1
          EndIf
       
          If (BoundaryMode_Right == 1) Then
             Q_boundary = Q_boundary+K_Soil(I_Soil)*DY(j)*DZ(k)*2./DX(i)*(Tave_Soil(i,j,k)-Tave_Vert(k))
          Else
             Q_Boundary = Q_Boundary
          EndIf
       EndDo
    EndDo
 EndDo

!Energy flow due to groundwater movement
 Do i = 1,NX_Tot
    Do j = NY_Tot,NY_Tot
       Do k = NZ_toWash+1,NZ_toWash+NZ_Wash
          Q_groundwater_y = Q_groundwater_y+Mdot_Groundwater_y*Cp_Groundwater*(Tave_Soil(i,j,k)-Tave_Vert(k))
       EndDo
    EndDo
 EndDo

 Do i = NX_Tot,NX_Tot
    Do j = 1,NY_Tot
       Do k = NZ_toWash+1,NZ_toWash+NZ_Wash
          Q_groundwater_x = Q_groundwater_x+Mdot_Groundwater_x*Cp_Groundwater*(Tave_Soil(i,j,k)-Tave_Vert(k))
       EndDo
    EndDo
 EndDo

!Conductive heat transfer across the top boundary
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Do k = 1,1
          Q_top = Q_top+K_Soil(1)*DX(i)*DY(j)*2./DZ(k)*(Tave_Soil(i,j,k)-Tsurf_o(i,j))
       EndDo
    EndDo
 EndDo

!Conductive heat transfer across the top boundary: insulated section
 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Do k = 1,1
          Q_top_ins = Q_top_ins+K_Soil(1)*DX(i)*DY(j)*2./DZ(k)*(Tave_Soil(i,j,k)-Tsurf_o(i,j))
       EndDo
    EndDo
 EndDo

!Conductive heat transfer across the deep earth boundary
 Q_bottom = 0.
 If (BoundaryMode_Bottom == 1) Then
    Do i = 1,NX_Tot
       Do j = 1,NY_Tot
          Do k = NZ_Tot,NZ_Tot
             Q_bottom = Q_bottom+K_Soil(N_Layers_Soil)*DX(i)*DY(j)*2./DZ(k)*(Tave_Soil(i,j,k)-T_Deep)
          EndDo
       EndDo
    EndDo
 EndIf

!Energy delivered to the flow stream
 Q_Fluid = Flow_in*DBLE(N_Inlets)*Cp_Fluid*(T_Fluid_In-T_Fluid_Out)

!Soil energy balance
 EB_Top = DABS(Qstore_T+Q_bottom+Q_top+Q_boundary+Q_groundwater_x+Q_groundwater_y-Q_Pipes)
 EB_Bottom = DABS(Q_bottom)+DABS(Q_top)+DABS(Q_boundary)+DABS(Q_groundwater_x)+DABS(Q_groundwater_y)+DABS(Q_Pipes)
 If (EB_Bottom > 10.) Then
    EB_Error = 100.*EB_Top/EB_Bottom
 Else
    EB_Error = 0.
 EndIf

!Fluid energy balance
 EB_Top = DABS(Qstore_f-Q_Fluid+Q_Pipes)
 EB_Bottom = DABS(Q_Fluid)+DABS(Q_Pipes)
 If (EB_Bottom > 10.) Then
    EB_Error_f = 100.*EB_Top/EB_Bottom
 Else
    EB_Error_f = 0.
 EndIf

!Insulated surface average temperature
 Area_1 = 0.
 Do i = NX_Beyond_L+1,NX_Tot-NX_Beyond_R
    Do j = NY_Beyond_B+1,NY_Tot-NY_Beyond_F
       Tave_Surf_i = Tave_Surf_i+DX(i)*DY(j)*Tsurf_i(i,j)
       Tave_Surf_o = Tave_Surf_o+DX(i)*DY(j)*Tsurf_o(i,j)
       Area_1 = Area_1+DX(i)*DY(j)
    EndDo
 EndDo
 Tave_Surf_i = Tave_Surf_i/Area_1
 Tave_Surf_o = Tave_Surf_o/Area_1

!Soil surface average temperature
 Area_Tot = 0.
 Do i = 1,NX_Tot
    Do j = 1,NY_Tot
       Tave_Surf_f = Tave_Surf_f+DX(i)*DY(j)*Tsurf_o(i,j)
       Area_Tot = Area_Tot+DX(i)*DY(j)
    EndDo
 EndDo
 Tave_Surf_f = (Tave_Surf_f-Area_1*Tave_Surf_o)/(Area_Tot-Area_1)

 Call SetOutputValue(1,T_Fluid_Out)
 Call SetOutputValue(2,getInputValue(2))
 Call SetOutputValue(3,Reynolds)
 Call SetOutputValue(4,Q_Pipes)
 Call SetOutputValue(5,Q_Fluid)
 Call SetOutputValue(6,Qstore_f)
 Call SetOutputValue(7,Qstore_T)
 Call SetOutputValue(8,Q_top)
 Call SetOutputValue(9,Q_top_ins)
 Call SetOutputValue(10,Q_bottom)
 Call SetOutputValue(11,Q_boundary)
 Call SetOutputValue(12,Q_groundwater_x+Q_groundwater_y)
 Call SetOutputValue(13,EB_Error)
 Call SetOutputValue(14,EB_Error_f)
 Call SetOutputValue(15,Tave_Surf_i)
 Call SetOutputValue(16,Tave_Surf_o)
 Call SetOutputValue(17,Tave_Surf_f)
 Call SetOutputValue(18,Qconvection_near)
 Call SetOutputValue(19,Qshortwave_near)
 Call SetOutputValue(20,Qlongwave_near)
 Call SetOutputValue(21,Qconduction_near)
 Call SetOutputValue(22,Qconvection_zone)
 Call SetOutputValue(23,Qshortwave_zone)
 Call SetOutputValue(24,Qlongwave_zone)
 Call SetOutputValue(25,Qconduction_zone)

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Everything is Done at this Iteration, Return to the Engine
 Return
 2500 Format(1000(F8.3,1X))
End
!-----------------------------------------------------------------------------------------------------------------------




/* One-Way, Fixed Effect (F), Balanced Replication, CRD */
DATA T0305;
  INFILE 'E:\Rt\ExpDes\T3.5.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A;
  MODEL Y = A;
  LSMEANS A / CL PDIFF;
  LSMEANS A / CL PDIFF ADJUST=TUKEY;
RUN;

/* One-Way, Fixed Effect (F), Unbalanced Replication, CRD */ 
DATA T0313;
  INFILE 'E:\Rt\ExpDes\T3.13.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A;
  MODEL Y = A;
  LSMEANS A / CL PDIFF;
RUN;

/* One-Way, Random Effect (R), Balanced Replication, CRD */
DATA E0304;
  INFILE 'E:\Rt\ExpDes\P3.4.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A;
  MODEL Y = A;
  RANDOM A / TEST;
  LSMEANS A / CL PDIFF;  /* WRONG REQUEST */
RUN;

PROC MIXED;
  CLASS A;
  MODEL Y =;
  RANDOM A;
RUN;

/* Two-Way, No Replication, CRD  */
DATA T0403;
  INFILE 'E:\Rt\ExpDes\T4.3.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B;
  LSMEANS A B / CL PDIFF;
RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B A*B;
  TEST H=A B E=A*B;
RUN;

/* Randomized Block Design, Two-Way, F+R, No-Repl, CRD */
DATA T0407;
  INFILE 'E:\Rt\ExpDes\T4.7.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B;
  RANDOM B / TEST;
  LSMEANS A / CL PDIFF;
RUN;

PROC GLM;
  CLASS A;
  MODEL Y = A;
  LSMEANS A / CL PDIFF;
RUN;


/* Two-Way, Fixed + Fixed, Balanced, CRD */
DATA T0506;
  INFILE 'E:\Rt\ExpDes\T5.6.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B A*B;
  LSMEANS A B A*B / CL PDIFF; /* A*B is only valid */
RUN;


/* Two-Way, Fixed + Fixed, Unbalanced, CRD */
/* Neter Table 23.1 */
DATA NT2301;
  INFILE 'E:\Rt\ExpDes\NT23.1.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B A*B;
  LSMEANS A B A*B / CL PDIFF; /* A*B is only valid */
RUN;


/* Three-Way, F+F+F, No Replication, CRD */
DATA T0605;
  INFILE 'E:\Rt\ExpDes\T6.5.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ C $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B C A*B A*C B*C;
RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B C A*B B*C;
  LSMEANS A B C A*B B*C/ CL PDIFF;
RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B C A*B A*C B*C A*B*C;
  LSMEANS A*B*C;
RUN;


/* One-Way, Fixed, Count Data, CRD */ 
DATA T0613;
  INFILE 'E:\Rt\ExpDes\T6.13.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ R $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A;
  MODEL Y = A;
RUN;


/* Two-Way, Fixed + Random, Count Data, Split-Plot */ 
DATA T0614;
  INFILE 'E:\Rt\ExpDes\T6.14.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ R $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B A*B;
  TEST H=A B E=A*B;
RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B;
  LSMEANS A B / CL PDIFF;
RUN;


/* Three-Way, F+F+F, Balanced, Split-Plot */
DATA T0704;
  INFILE 'E:\Rt\ExpDes\T7.4.CSV' FIRSTOBS=2 DLM=",";
  INPUT R $ A $ B $ C $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS R A B C;
  MODEL Y = R A B A*B R*A*B C A*C B*C A*B*C;
  RANDOM R R*A*B / TEST;
RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B C A*C;
  LSMEANS A*C / CL PDIFF;
RUN;


/* Three-Way, F+F+F, Balanced, Split-Plot */
DATA T0705;
  INFILE 'E:\Rt\ExpDes\T7.5.CSV' FIRSTOBS=2 DLM=",";
  INPUT R $ A $ B $ C $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS R A B C;
  MODEL Y = R A B A*B R*A*B C A*C B*C A*B*C;
  RANDOM R R*A*B / TEST;
RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B C;
  LSMEANS A B C / CL PDIFF;
RUN;


/* Two-Way, Fixed+Fixed, Balanced, Split-Plot */
DATA TP213;
  INFILE 'E:\Rt\ExpDes\TP213.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ R $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B;
  MODEL Y = A B A*B;
  TEST H=A B E=A*B;
RUN;


/* Three-Way, R+R+R, Nested */
DATA T0714;
  INFILE 'E:\Rt\ExpDes\T7.14.CSV' FIRSTOBS=2 DLM=",";
  INPUT A $ B $ C $ R $ Y;
PROC PRINT; RUN;

PROC GLM;
  CLASS A B C;
  MODEL Y = A B(A) C(A*B);
  RANDOM A B(A) C(A*B) / TEST;
RUN;



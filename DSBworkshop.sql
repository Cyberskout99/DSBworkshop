--DROP TABLE Demo_tbl CASCADE;
CREATE TABLE Demo_tbl (
  PTID varchar (12) PRIMARY KEY,
  PtLAST varchar(12) NOT NULL,
  PtFIRST varchar (12) NOT NULL,
  DOB date NOT NULL,
  Sex char(1) NOT NULL,
  Coverage varchar (5),
  DNA_ID char(42)
);

-- Copying in from CSV template
-- The Copy function will write in the PTID serial variable (PK) automatically
-- Field names are not case sensitive
COPY Demo_tbl(ptid,ptlast,ptfirst,dob,sex,coverage,dna_id)
FROM 'C:\Users\Paul\Desktop\DSB Workshop\BLA_DiabDems.csv' DELIMITER ',' CSV HEADER;

--DROP TABLE GTT2_tbl CASCADE;
CREATE TABLE GTT2_tbl (
  GTTID serial PRIMARY KEY,
  PTID varchar (12) NOT NULL REFERENCES Demo_tbl(PTID),
  CollectDate date NOT NULL,
  Gluc0hr integer,
  Gluc2hr integer
);

COPY GTT2_tbl(ptid,collectdate,gluc0hr,gluc2hr)
FROM 'C:\Users\Paul\Desktop\DSB Workshop\BLA_DiabGTT.csv' DELIMITER ',' CSV HEADER;

--DROP TABLE Labs_tbl;
CREATE TABLE Labs_tbl (
  LabsID serial PRIMARY KEY,
  PTID varchar (12) NOT NULL REFERENCES Demo_tbl(PTID),
  Collected date NOT NULL,
  HbA1c numeric,
  GlucF integer,
  BMI numeric,
  ESR integer,
  VitD numeric,
  GlucR integer
);
COPY Labs_tbl(ptid,collected,hba1c,glucf,bmi,esr,vitd,glucr)
FROM 'C:\Users\Paul\Desktop\DSB Workshop\BLA_DiabLabs.csv' DELIMITER ',' CSV HEADER;

--DROP TABLE LabsPost_tbl;
CREATE TABLE LabsPost_tbl (
  LabsPostID serial PRIMARY KEY,
  PTID varchar (12) NOT NULL REFERENCES Demo_tbl(PTID),
  CollectDate date NOT NULL,
  HgbA1c numeric,
  FastGluc integer,
  BMI numeric,
  SedRate integer,
  VitD numeric,
  RandGluc integer
);
COPY LabsPost_tbl(ptid,collectdate,hgba1c,fastgluc,bmi,sedrate,vitd,randgluc)
FROM 'C:\Users\Paul\Desktop\DSB Workshop\BLA_DiabLabsPost.csv' DELIMITER ',' CSV HEADER;

-- DROP TABLE Genetics_tbl;
CREAT TABLE Genetics_tbl (
);

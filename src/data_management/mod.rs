/////////////////////////////////////////////////////////////////////////////////////
//
// Contagion model
//
// data_managment module
//
// functions to read and write from file system
//
////////////////////////////////////////////////////////////////////////////////////

extern crate yaml_rust;

use super::world;
use std::fmt;
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::prelude::{Read, Write};
use std::path::PathBuf;
use std::str::FromStr;
use yaml_rust::yaml;

// -------------------------------- File paths -------------------------------------------------------------
pub struct ModelDataStore {
    parameter_file: PathBuf,
    scenario_dir: PathBuf,
    pub person_log_file: Option<PersonLog>,
}

impl ModelDataStore {
    // create file paths starting at model_root
    // panic if root does not exist or if parameter file does not exist
    // create scenario folder if need be
    pub fn new(model_root: &str) -> ModelDataStore {
        println!("model root dir: {}", model_root);
        let model_dir = PathBuf::from(model_root);
        println!("model root path: {}", model_dir.as_path().display());
        let parameter_file: PathBuf = [model_root, "parms.yaml"].iter().collect();
        let scenario_dir: PathBuf = [model_root, "Scenarios"].iter().collect();

        // create the scenario directory - delete first if it exists
        if scenario_dir.exists() {
            match fs::remove_dir_all(&scenario_dir) {
                Ok(_) => (),
                Err(msg) => panic!(
                    "Could not delete pre-existing scenario directory {} - {}",
                    &scenario_dir.display(),
                    msg
                ),
            }
        }
        match fs::create_dir(&scenario_dir) {
            Ok(_) => (),
            Err(msg) => panic!(
                "Could not create scenario directory {} - {}",
                &scenario_dir.display(),
                msg
            ),
        }

        ModelDataStore {
            parameter_file: parameter_file,
            scenario_dir: scenario_dir,
            person_log_file: None,
        }
    }

    pub fn get_model_parms(&self) -> ModelParameters {
        let mut parm_file = match File::open(self.parameter_file.as_path()) {
            Ok(f) => f,
            Err(msg) => panic!(
                "Cannot open parm file {} - {}",
                self.parameter_file.display(),
                msg
            ),
        };

        let mut parm_string = String::new();
        parm_file.read_to_string(&mut parm_string).unwrap();

        // the loader creates an array of yaml enums
        let docs = match yaml::YamlLoader::load_from_str(&parm_string) {
            Ok(docs_vec) => docs_vec,
            Err(_) => panic!("error opening parameter file"),
        };

        // there can be multiple docs in Yaml file.  Only the first one interests us.
        let doc = &docs[0];

        // parse the doc
        let model_name = doc["model_name"].as_str().unwrap();
        let model_description = doc["model_description"].as_str().unwrap();
        let cycles = doc["cycles"].as_i64().unwrap() as usize;

        // parse - Scenario parms ---------------------------------------------------
        let sp_key = "scenario_parms";
        let variable = doc[sp_key]["scenario_variable"].as_str().unwrap();
        let stat_type = doc[sp_key]["scenario_stat_type"]
            .as_str()
            .expect("YAML - Expected 'scenario_stat_type' Parameter");
        let mut scenario_values: Vec<f64> = Vec::new();
        for v in doc[sp_key]["scenario_values"].as_vec().unwrap() {
            scenario_values.push(
                v.as_f64()
                    .expect("YAML - Expected 'scenario_values' parameter"),
            );
        }

        // parse World parms -----------------------------------------------------
        let wp_key = "world_parms";
        let rows = doc[wp_key]["rows"].as_i64().unwrap() as usize;
        let cols = doc[wp_key]["cols"].as_i64().unwrap() as usize;
        let population = doc[wp_key]["population_size"].as_i64().unwrap() as usize;

        // parse - random number parms
        let sp_key = "sim_parms";
        let mobility = (
            doc[sp_key]["mobility"]["mean"].as_f64().unwrap(),
            doc[sp_key]["mobility"]["sample_population"]
                .as_i64()
                .unwrap() as usize,
        );
        let exposure = (
            doc[sp_key]["exposure"]["mean"].as_f64().unwrap(),
            doc[sp_key]["exposure"]["sample_population"]
                .as_i64()
                .unwrap() as usize,
        );
        let compliability = (
            doc[sp_key]["compliability"]["mean"].as_f64().unwrap(),
            doc[sp_key]["compliability"]["sample_population"]
                .as_i64()
                .unwrap() as usize,
        );
        let immunity_loss = (
            doc[sp_key]["immunity_loss"]["mean"].as_f64().unwrap(),
            doc[sp_key]["immunity_loss"]["sample_population"]
                .as_i64()
                .unwrap() as usize,
        );
        let days_infected = (
            doc[sp_key]["days_infected"]["min"].as_f64().unwrap(),
            doc[sp_key]["days_infected"]["max"].as_f64().unwrap(),
            doc[sp_key]["days_infected"]["mode"].as_f64().unwrap(),
        );
        let days_symptomatic = (
            doc[sp_key]["days_symptomatic"]["min"].as_f64().unwrap(),
            doc[sp_key]["days_symptomatic"]["max"].as_f64().unwrap(),
            doc[sp_key]["days_symptomatic"]["mode"].as_f64().unwrap(),
        );
        let days_hospitalized = (
            doc[sp_key]["days_hospitalized"]["min"].as_f64().unwrap(),
            doc[sp_key]["days_hospitalized"]["max"].as_f64().unwrap(),
            doc[sp_key]["days_hospitalized"]["mode"].as_f64().unwrap(),
        );

        //TODO this code looks clumsy - there must be a more direct way
        let mut vulnerability: Vec<f64> = Vec::new();
        for v in doc[sp_key]["vulnerability"]["weightings"].as_vec().unwrap() {
            vulnerability.push(v.as_f64().unwrap());
        }

        ModelParameters::new(
            String::from(model_name),
            String::from(model_description),
            cycles,
            rows,
            cols,
            population,
            mobility,
            exposure,
            compliability,
            immunity_loss,
            vulnerability,
            days_infected,
            days_symptomatic,
            days_hospitalized,
            variable,
            stat_type,
            scenario_values,
        )
    }

    fn get_scenario_directory(&self, scenario_number: usize) -> PathBuf {
        let dir_path = PathBuf::from(format!("scenario_{:04}", scenario_number));
        let dir_full_path: PathBuf = [&self.scenario_dir, &dir_path].iter().collect();
        dir_full_path
    }

    pub fn create_scenario_directory(&mut self, scenario_number: usize) -> String {
        // create the scenario directory
        let dir_full_path = self.get_scenario_directory(scenario_number);
        if !dir_full_path.exists() {
            match fs::create_dir(&dir_full_path) {
                Ok(_) => (),
                Err(msg) => panic!(
                    "Could not create scenario directory {} - {}",
                    &dir_full_path.display(),
                    msg
                ),
            }
        }

        // open the scenario log file and return a pointer
        dir_full_path.display().to_string()
    }
}

// ----------------------------- Scenario parameters -------------------------------------------------------
//
//  Scenario parameters manage a series of runs each which evolves in some way from the previous run
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum StatType {
    Mean,
    Mode,
    SamplePopulation,
}
impl fmt::Display for StatType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl FromStr for StatType {
    type Err = ();

    fn from_str(s: &str) -> Result<StatType, ()> {
        match s {
            "mean" => Ok(StatType::Mean),
            "mode" => Ok(StatType::Mode),
            "sample_population" => Ok(StatType::SamplePopulation),
            _ => Err(()),
        }
    }
}

#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum SensitivityVariable {
    Mobility,
    Exposure,
    Compliability,
    ImmunityLossRate,
    Vulnerability,
    DaysInfected,
    DaysSymptomatic,
    DaysHospitalized,
}
impl fmt::Display for SensitivityVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl FromStr for SensitivityVariable {
    type Err = ();

    fn from_str(s: &str) -> Result<SensitivityVariable, ()> {
        match s {
            "mobility" => Ok(SensitivityVariable::Mobility),
            "exposure" => Ok(SensitivityVariable::Exposure),
            "compliability" => Ok(SensitivityVariable::Compliability),
            "immunity_loss_rate" => Ok(SensitivityVariable::ImmunityLossRate),
            "vulnerability" => Ok(SensitivityVariable::Vulnerability),
            "days_infected" => Ok(SensitivityVariable::DaysInfected),
            "days_symptomatic" => Ok(SensitivityVariable::DaysSymptomatic),
            "days_hospitalized" => Ok(SensitivityVariable::DaysHospitalized),
            _ => Err(()),
        }
    }
}

pub struct ModelParameters {
    pub model_name: String,
    pub model_description: String,
    pub cycles: usize,
    rows: usize,
    cols: usize,
    population: usize,
    mobility_parms: (f64, usize),             //mean, sample_population,
    exposure_parms: (f64, usize),             //mean, sample_population,
    compliability_parms: (f64, usize),        //mean, sample_population,
    immunity_loss_rate_parms: (f64, usize),   //mean, sample_population,
    vulnerability_parms: Vec<f64>,            //I->W, I->S, S->W, S->H, S->D, H->D TODO,
    days_infected_parms: (f64, f64, f64),     //min, max, mode,
    days_symptomatic_parms: (f64, f64, f64),  //min, max, mode,
    days_hospitalized_parms: (f64, f64, f64), //min, max, mode,
    pub variable: SensitivityVariable,
    pub stat_type: StatType,
    pub value_vector: Vec<f64>,
    pub current_increment: Option<usize>,
}

impl ModelParameters {
    pub fn new(
        model_name: String,
        model_description: String,
        cycles: usize,
        rows: usize,
        columns: usize,
        population: usize,
        mobility_parms: (f64, usize),      //mean, sample_population,
        exposure_parms: (f64, usize),      //mean, sample_population,
        compliability_parms: (f64, usize), //mean, sample_population,
        immunity_loss_rate_parms: (f64, usize), //mean, sample_population,
        vulnerability_parms: Vec<f64>,     //I->W, I->S, S->W, S->H, S->D, H->D TODO,
        days_infected_parms: (f64, f64, f64), //min, max, mode,
        days_symptomatic_parms: (f64, f64, f64), //min, max, mode,
        days_hospitalized_parms: (f64, f64, f64), //min, max, mode,
        variable_str: &str,                // variable to be altered from one run to the next
        statistic_str: &str, // statistic to be varied from one run to the next (e.g mean)
        value_vector: Vec<f64>, // values to be tested in each scnario run
    ) -> ModelParameters {
        let variable: SensitivityVariable = match variable_str.parse() {
            Ok(x) => x,
            Err(_) => panic!("Unknown variable in parameter definition {}", variable_str),
        };

        let stat_type: StatType = match statistic_str.parse() {
            Ok(x) => x,
            Err(_) => panic!(
                "Unknown statistic in parameter definition {}",
                statistic_str
            ),
        };

        ModelParameters {
            model_name: model_name,
            model_description: model_description,
            cycles: cycles,
            rows: rows,
            cols: columns,
            population: population,
            mobility_parms: mobility_parms,
            exposure_parms: exposure_parms,
            compliability_parms: compliability_parms,
            immunity_loss_rate_parms: immunity_loss_rate_parms,
            vulnerability_parms: vulnerability_parms,
            days_infected_parms: days_infected_parms,
            days_symptomatic_parms: days_symptomatic_parms,
            days_hospitalized_parms: days_hospitalized_parms,
            variable: variable,
            stat_type: stat_type,
            value_vector: value_vector,
            current_increment: None,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "Model name {}\nModel description {}\nPopulation {}\nSensitivity analysis on {} {} with values {:?}",
            self.model_name, self.model_description, self.population, self.variable, self.stat_type, self.value_vector
        )
    }
}

// the Iterator trait for scenario parameters issues
// a sequence of WorldParms structs each one differering only by an increment
// of the variable being sensitivity tested
impl Iterator for ModelParameters {
    type Item = world::WorldParms;

    fn next(&mut self) -> Option<world::WorldParms> {
        match self.current_increment {
            Some(increment) => self.current_increment = Some(increment + 1),
            None => self.current_increment = Some(0),
        }

        if self.current_increment.unwrap() == self.value_vector.len() {
            return None;
        }

        let new_parm_value: f64 = self.value_vector[self.current_increment.unwrap()];

        match self.variable {
            SensitivityVariable::Mobility => match self.stat_type {
                StatType::Mean => self.mobility_parms.0 = new_parm_value,
                StatType::Mode => panic!("Mobility variable has no mode parameter"),
                StatType::SamplePopulation => self.mobility_parms.1 = new_parm_value as usize,
            },
            SensitivityVariable::Exposure => match self.stat_type {
                StatType::Mean => self.exposure_parms.0 = new_parm_value,
                StatType::Mode => panic!("Exposure variable has no mode parameter"),
                StatType::SamplePopulation => self.exposure_parms.1 = new_parm_value as usize,
            },
            SensitivityVariable::Compliability => match self.stat_type {
                StatType::Mean => self.compliability_parms.0 = new_parm_value,
                StatType::Mode => panic!("Compliability variable has no mode parameter"),
                StatType::SamplePopulation => self.compliability_parms.1 = new_parm_value as usize,
            },
            SensitivityVariable::ImmunityLossRate => match self.stat_type {
                StatType::Mean => self.immunity_loss_rate_parms.0 = new_parm_value,
                StatType::Mode => panic!("Immunity loss variable has no mode parameter"),
                StatType::SamplePopulation => {
                    self.immunity_loss_rate_parms.1 = new_parm_value as usize
                }
            },
            SensitivityVariable::DaysInfected => match self.stat_type {
                StatType::Mode => self.days_infected_parms.0 = new_parm_value,
                _ => panic!("Days infected variable mode is only available statistic type"),
            },
            SensitivityVariable::DaysSymptomatic => match self.stat_type {
                StatType::Mode => self.days_symptomatic_parms.0 = new_parm_value,
                _ => panic!("Days symptomatic variable mode is only available statistic type"),
            },
            SensitivityVariable::DaysHospitalized => match self.stat_type {
                StatType::Mode => self.days_hospitalized_parms.0 = new_parm_value,
                _ => panic!("Days hospitalized variable mode is only available statistic type"),
            },
            _ => (),
        }

        Some(world::WorldParms::new(
            self.rows,
            self.cols,
            self.population,
            world::SimParms::new(
                self.mobility_parms,
                self.exposure_parms,
                self.compliability_parms,
                self.immunity_loss_rate_parms,
                &self.vulnerability_parms,
                self.days_infected_parms,
                self.days_symptomatic_parms,
                self.days_hospitalized_parms,
            ),
        ))
    }
}

// ----------------------------- Output model results ------------------------------------------------------
pub struct PersonLog {
    file_path_create: String,
    file_path_move: String,
    file_path_disease: String,
}

impl PersonLog {
    pub fn new(file_path_str: &str) -> PersonLog {
        // create files and write header lines
        let mut file_path_create = PathBuf::from(file_path_str);
        file_path_create.push("person_log_create.csv");
        let mut file = match File::create(&file_path_create) {
            Err(why) => panic!("Couldn't create {} - {}", file_path_create.display(), why),
            Ok(file) => file,
        };

        match file.write_all(
            b"person_id,cycle,cell_id,mobility, exposure, compliability, immunity, vulnerability",
        ) {
            Err(why) => panic!(
                "Couldn't write headers to {} - {}",
                file_path_create.display(),
                why
            ),
            Ok(_) => (),
        }
        file.sync_all()
            .expect(&format!("Could not sync {}", file_path_create.display()));

        let mut file_path_move = PathBuf::from(file_path_str);
        file_path_move.push("person_log_move.csv");
        let mut file = match File::create(&file_path_move) {
            Err(why) => panic!("Couldn't create {} - {}", file_path_move.display(), why),
            Ok(file) => file,
        };

        match file.write_all(b"person_id,cycle,from_cell_id,to_cell_id") {
            Err(why) => panic!(
                "Couldn't write headers to {} - {}",
                file_path_move.display(),
                why
            ),
            Ok(_) => (),
        }
        file.sync_all()
            .expect(&format!("Could not sync {}", file_path_move.display()));

        let mut file_path_disease = PathBuf::from(file_path_str);
        file_path_disease.push("person_log_disease.csv");
        let mut file = match File::create(&file_path_disease) {
            Err(why) => panic!("Couldn't create {} - {}", file_path_disease.display(), why),
            Ok(file) => file,
        };

        match file.write_all(b"person_id,cycle,from_cell_id,to_cell_id") {
            Err(why) => panic!(
                "Couldn't write headers to {} - {}",
                file_path_disease.display(),
                why
            ),
            Ok(_) => (),
        }
        file.sync_all()
            .expect(&format!("Could not sync {}", file_path_disease.display()));

        PersonLog {
            file_path_create: file_path_create.display().to_string(),
            file_path_move: file_path_move.display().to_string(),
            file_path_disease: file_path_disease.display().to_string(),
        }
    }

    pub fn append(&self, populace_log_records: Vec<&Vec<world::PersonLogEntry>>) {
        // open files
        let mut file_create = OpenOptions::new()
            .write(true)
            .create(true)
            .append(false)
            .open(&self.file_path_create)
            .unwrap();
        let mut file_move = OpenOptions::new()
            .write(true)
            .create(true)
            .append(false)
            .open(&self.file_path_move)
            .unwrap();
        let mut file_disease = OpenOptions::new()
            .write(true)
            .create(true)
            .append(false)
            .open(&self.file_path_disease)
            .unwrap();

        // write header lines
        file_create
            .write_all(
                b"id,cycle,current_cell,mobility,exposure,compliability,immunity,vulnerability\n",
            )
            .expect(&format!(
                "Could not write header to {}",
                self.file_path_create
            ));
        file_move
            .write_all(b"id,cycle,current_cell,to_cell\n")
            .expect(&format!(
                "Could not write header to {}",
                self.file_path_move
            ));
        file_disease
            .write_all(b"id,cycle,current_cell,new_status\n")
            .expect(&format!(
                "Could not write header to {}",
                self.file_path_disease
            ));

        // write log entries into appropriate file
        for person_log_records in populace_log_records {
            for r in person_log_records {
                match r.event {
                    world::PersonEvent::Create(
                        mobility,
                        exposure,
                        compliability,
                        immunity,
                        vulnerability,
                    ) => {
                        file_create
                            .write_all(
                                format!(
                                    "{},{},{},{:.6},{:.6},{:.6},{:.6},{}\n",
                                    r.id,
                                    r.cycle,
                                    r.current_cell,
                                    mobility,
                                    exposure,
                                    compliability,
                                    immunity,
                                    vulnerability
                                )
                                .as_bytes(),
                            )
                            .expect(&format!("Could not write to {}", self.file_path_create));
                    }
                    world::PersonEvent::Move(to_cell) => {
                        file_move
                            .write_all(
                                format!("{},{},{},{}\n", r.id, r.cycle, r.current_cell, to_cell,)
                                    .as_bytes(),
                            )
                            .expect(&format!("Could not write to {}", self.file_path_move));
                    }
                    world::PersonEvent::DiseaseStatusChange(new_status) => {
                        file_disease
                            .write_all(
                                format!("{},{},{},{}\n", r.id, r.cycle, r.current_cell, new_status,)
                                    .as_bytes(),
                            )
                            .expect(&format!("Could not write to {}", self.file_path_disease));
                    }
                }
            }
        }

        // flush files
        file_create
            .sync_all()
            .expect(&format!("Could not sync {}", self.file_path_create));
        file_move
            .sync_all()
            .expect(&format!("Could not sync {}", self.file_path_move));
        file_disease
            .sync_all()
            .expect(&format!("Could not sync {}", self.file_path_disease));
    }
}

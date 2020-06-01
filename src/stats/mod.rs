/////////////////////////////////////////////////////////////////////////////////////
//
// Contagion model
//
// stats module
//
// calculates statistics
//
////////////////////////////////////////////////////////////////////////////////////
use super::world;
use csv::Writer;
use csv::WriterBuilder;
use std::collections::HashMap;
use std::error::Error;
use std::fs::{File, OpenOptions};
use std::io::prelude::Write;
use std::path::Path;

pub struct CellLog {
    file_path: String,
}

impl CellLog {
    pub fn new(file_path: &str) -> CellLog {
        let path = Path::new(file_path);
        let mut file = match File::create(&path) {
            Err(why) => panic!("Couldn't create {} - {}", path.display(), why),
            Ok(file) => file,
        };

        match file.write_all(world::CellSummary::header()) {
            Err(why) => panic!("Couldn't write headers to {} - {}", path.display(), why),
            Ok(_) => (),
        }
        CellLog {
            file_path: String::from(file_path),
        }
    }

    pub fn append(&self, cell_records: &Vec<world::CellSummary>) {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open(&self.file_path)
            .unwrap();
        let mut wtr = WriterBuilder::new().has_headers(false).from_writer(file);

        for record in cell_records {
            wtr.serialize(record).unwrap();
        }
        wtr.flush().unwrap();
        // Ok(())
    }
}

// pub fn population_table(world: &world::Region, file: &str) -> Result<(), Box<dyn Error>> {
//     let file_path = Path::new(file);
//     let mut wtr = Writer::from_path(file_path)?;

//     // wtr.write_record(
//     //                 &["CellIndex",
//     //                   "Row",
//     //                   "Col",
//     //                   "Virality",
//     //                   "TotalPop",
//     //                   "HomePop",
//     //                   "AwayPop",
//     //                   "WellPop",
//     //                   "ImmunePop",
//     //                   "Infected",
//     //                   "Symptomatic",
//     //                   "Dead"])?;

//     let cell_summaries = &world.summarize_by_cell();
//     for cs in cell_summaries {
//         wtr.serialize(cs)?;
//     }
//     wtr.flush()?;
//     Ok(())
// }

pub struct TransitionLog {
    file_path: String,
}

impl TransitionLog {
    pub fn new(file_path: &str) -> TransitionLog {
        let path = Path::new(file_path);
        let mut file = match File::create(&path) {
            Err(why) => panic!("Couldn't create {} - {}", path.display(), why),
            Ok(file) => file,
        };

        match file.write_all(b"cycle,cell,row_type,row_subtype,from_disease_status,to_disease_status,pop_quantity,avg_virality,avg_immunity\n") {
            Err(why) => panic!("Couldn't write headers to {} - {}", path.display(), why),
            Ok(_) => ()
        }
        TransitionLog {
            file_path: String::from(file_path),
        }
    }

    pub fn append(&self, transition_records: &HashMap<world::CycleRecordKey, world::CycleRecord>) {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open(&self.file_path)
            .unwrap();
        let mut wtr = WriterBuilder::new().has_headers(false).from_writer(file);

        for record in transition_records.values() {
            wtr.serialize(record).unwrap();
        }
        wtr.flush().unwrap();
        // Ok(())
    }
}

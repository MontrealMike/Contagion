use std::env;
use std::io::{self, Write};

mod data_management;
mod world;

fn main() {
    // process command line arguments (for now just the model root directory location)
    let args: Vec<_> = env::args().collect();
    let model_root = if args.len() > 1 {
        &args[1]
    } else {
        panic! {"Error: no model location specified"}
    };

    // The model data store handles all models inputs and outputs
    let mut model_data_store = data_management::ModelDataStore::new(model_root);

    // create the simulation parameters
    let model_parms = model_data_store.get_model_parms();
    println!("\n--------------------Contagion Model-----------------------");
    println!("{}", model_parms.to_string());
    println!(
        "Model run {} scenarios of {} cycles",
        model_parms.value_vector.len(),
        model_parms.cycles
    );
    let cycle_ceiling = model_parms.cycles + 1; // cycles are 1 - based
    let mut scenario_number = 0; // scenario counter TODO #7 make part of iterator

    // loop around scenarios
    for model_scenario in model_parms {
        scenario_number += 1;

        // initialize the scenario
        println!(
            "\nStarting scenario {} ------------------------------------------------------------",
            scenario_number
        );
        let scenario_dir_name = model_data_store.create_scenario_directory(scenario_number);
        let person_log = data_management::PersonLog::new(&scenario_dir_name);
        let mut world = create_world(model_scenario);

        world.seed_infection(0, 10); // infect 10 people

        // Loop around cycles
        // TODO #3 add timer
        print!("Running {} cycles", cycle_ceiling - 1);
        for cycle in 1..cycle_ceiling {
            world.move_people(cycle);
            world.update_disease_stati(cycle);
            print!(".");
            io::stdout().flush().unwrap();
        }

        println!();
        print!("Logging scenario events ... ");
        io::stdout().flush().unwrap();
        save_person_logs(&world.populace, &person_log);
        println!("done");
        io::stdout().flush().unwrap();
    }
}

fn create_world(parms: world::WorldParms) -> world::Region {
    world::Region::new(
        parms.rows,
        parms.cols,
        parms.people_count,
        1,
        parms.sim_parms,
    )
}

// TODO #6 this whole person log thing is quite awkward - why can't it be created within this
// function
fn save_person_logs(populace: &Vec<world::Person>, person_log_file: &data_management::PersonLog) {
    // create a vector of vector of personal logs
    let mut populace_log: Vec<&Vec<world::PersonLogEntry>> = Vec::with_capacity(populace.len());
    for p in populace {
        populace_log.push(&p.event_log);
    }
    person_log_file.append(populace_log);
}

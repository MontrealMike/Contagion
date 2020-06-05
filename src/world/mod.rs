/////////////////////////////////////////////////////////////////////////////////////
//
// Contagion model
//
// world module
//
// the world is defined as a grid of cells (called a region)
// each cell contains a populace
//
// In each cycle - people move, change their disease status, get tested
//
////////////////////////////////////////////////////////////////////////////////////

use rand::prelude::*;
use rand::seq::SliceRandom;
use rand_distr::{Beta, Distribution, Pert};
use serde::Serialize;
use std::fmt;
use std::io::{self, Write};

// Region ---------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Region<'a> {
    // pub populace: Vec<Person>,
    cells: Vec<Cell<'a>>,
    cell_home_population: Vec<usize>,
    // cell_virality: Vec<f64>,
    od_attraction_matrix: Vec<Vec<f64>>,
    population_count: usize,
    cell_count: usize,
    row_count: usize,
    col_count: usize,
    sim_parms: SimParms,
}

impl<'a> Region<'a> {
    pub fn new(
        row_count: usize,
        col_count: usize,
        people_count: usize,
        empty_pop_pull_factor: usize,
        initial_infected_count: usize,
        sim_parms: SimParms,
    ) -> Region<'a> {
        // creating the cells
        let cell_count = row_count * col_count;
        let mut cells: Vec<Cell> = Vec::with_capacity(cell_count);
        for cell_index in 0..cell_count {
            cells.push(Cell::new(cell_index));
        }

        // populating the cells
        // let mut cell_pops: Vec<usize> = vec![0; cell_count];
        let mut neighbourhoods: Vec<Vec<usize>> = Vec::with_capacity(cell_count);
        let mut neighbourhood_pops: Vec<usize> = vec![empty_pop_pull_factor; cell_count];

        // populate neighbourhood vector and initialize neighbourhood populations
        print!("     creating neighbourhoods ...");
        io::stdout().flush().unwrap();
        for index in 0..cell_count {
            let neighbours = CellAddress::new(index, row_count, col_count).neighbours();
            neighbourhoods.push(neighbours);
        }
        println!(" {} created", neighbourhoods.len());

        // randomly pick those who are infected at the outset
        let mut rng = rand::thread_rng();
        let person_ids: Vec<usize> = (0..people_count).collect();
        let infectee_ids: Vec<usize> = person_ids
            .choose_multiple(&mut rng, initial_infected_count)
            .cloned()
            .collect();

        // randomly spread people into cells - cells with people attract more people
        print!("     creating people ...");
        io::stdout().flush().unwrap();
        let mut rng = thread_rng(); // prep random # generator (I think)
        for p in 0..people_count {
            let wi = rand::distributions::WeightedIndex::new(&neighbourhood_pops).unwrap();
            let cell_choice = wi.sample(&mut rng);
            // cell_pops[cell_choice] += 1;
            for neighbour in &neighbourhoods[cell_choice] {
                neighbourhood_pops[*neighbour] += 1;
            }
            let new_person = Person::new(p, &sim_parms, cell_choice);
            if infectee_ids.len() > 0 {
                if p == infectee_ids[0] {
                    new_person.update_disease_status(0, &sim_parms);
                    print!("i")
                }
            }
            cells[cell_choice].add_resident(new_person);
        }
        println!("   {} people created", people_count);

        // create od attraction matrix - based on distances & home populations
        // so it can be calculated ahead of time since they are fixed
        print!("     creating od matrix ...");
        io::stdout().flush().unwrap();
        let mut od_matrix: Vec<Vec<f64>> = Vec::with_capacity(cell_count);
        for from_index in 0..cell_count {
            let from_row: usize = from_index / col_count;
            let from_col: usize = from_index % col_count;
            let mut od_matrix_row: Vec<f64> = Vec::with_capacity(cell_count);
            for to_index in 0..cell_count {
                let to_row: usize = to_index / col_count;
                let to_col: usize = to_index % col_count;
                let attraction: f64 = if from_index <= to_index {
                    let distance_sq: f64 = if from_index == to_index {
                        0.5 * 0.5 //magic number: people are assumed to be 1/2 unit from centre
                    } else {
                        (to_row as f64 - from_row as f64).powi(2)
                            + (to_col as f64 - from_col as f64).powi(2)
                    };
                    ((cells[from_index].resident_pop() * cells[to_index].resident_pop()) + 1) as f64
                        / distance_sq as f64 // magic number 1 to endure non-zero
                } else {
                    od_matrix[to_index][from_index] //get the value on the other side of the diagonal
                };
                od_matrix_row.push(attraction);
            }
            od_matrix.push(od_matrix_row);
        }
        println!(
            "   {} cell od attraction matrix created",
            od_matrix.len().pow(2)
        );

        Region {
            // populace: populace,
            cells: cells,
            // cell_home_population: cell_pops, //BUG #5 contains empty cell pop!
            // cell_virality: cell_virality,
            od_attraction_matrix: od_matrix,
            population_count: people_count,
            cell_count: cell_count,
            row_count: row_count,
            col_count: col_count,
            sim_parms: sim_parms,
        }
    }

    // infection in a count number of randomly selected people
    // pub fn seed_infection(&mut self, cycle: usize, count: usize) {
    //     let mut rng = rand::thread_rng();
    //     let person_ids: Vec<usize> = (0..self.population_count).collect();
    //     let infectee_ids = person_ids.choose_multiple(&mut rng, count).cloned();

    //     print!("     seeding the infection ... ");
    //     io::stdout().flush().unwrap();
    //     let person_count: usize = 0;
    //     for person_index in infectee_ids {
    //         self.populace[person_index].update_disease_status(cycle, &self.sim_parms);
    //     }
    //     println!(" {} people infected", count);
    // }

    // Moves people from one cell to another based on their current location,
    // their home cell, their mobility factor
    // Only non-hospitalized alive people move
    // Cell virality levels are updated to reflect the current population
    // (TODO) #1 add a dampening measures in on symptomatic persons
    pub fn move_people(&mut self, cycle: usize) {
        let mut cell_current_pop: Vec<usize> = vec![0; self.cell_count];
        let mut cell_tot_virality: Vec<f64> = vec![0.0; self.cell_count];
        let mut rng = rand::thread_rng();

        // we have an arrivals and departures row for each cell
        for cell in self.cells {
            for person in cell.populace.iter_mut() {
                // more mobile people at home tend to move away more
                // less mobile people away tend to return home more
                let will_move = if person.disease_status == DiseaseStatus::Hospitalized
                    || person.disease_status == DiseaseStatus::Dead
                {
                    false
                } else {
                    if person.current_cell == person.home_cell {
                        person.mobility > rng.gen::<f64>()
                    } else {
                        person.mobility <= rng.gen::<f64>()
                    }
                };

                if will_move {
                    // use the attraction row from the person's home cell
                    let wi = rand::distributions::WeightedIndex::new(
                        &self.od_attraction_matrix[person.home_cell],
                    )
                    .unwrap();
                    let to_cell_index = wi.sample(&mut rng);
                    person.relocate(cycle, to_cell_index);
                }

                // cumulate virality levels
                cell_current_pop[person.current_cell] += 1;
                if person.disease_status == DiseaseStatus::Infected
                    || person.disease_status == DiseaseStatus::Symptomatic
                {
                    cell_tot_virality[person.current_cell] +=
                        person.exposure * (1.0 - person.immunity);
                }
            }
            // update cell virality
        }

        // function yields virality score between 0 and 1.
        // This is a diminishing returns function.
        for cell_index in 0..self.cell_count {
            self.cell_virality[cell_index] = 1.0 - 1.0 / (cell_tot_virality[cell_index] + 1.0);
        }
    }
    pub fn update_disease_stati(&mut self, cycle: usize) {
        let mut rng = rand::thread_rng();
        for person in self.populace.iter_mut() {
            // determine new status
            let random_number = rng.gen::<f64>();
            if person.disease_status == DiseaseStatus::Well {
                let prob_infection = person.exposure
                    * self.cell_virality[person.current_cell]
                    * (1.0 - person.immunity);
                person.immunity *= 1.0 - self.sim_parms.immunity_loss.sample(&mut rng);
                if random_number <= prob_infection {
                    person.update_disease_status(cycle, &self.sim_parms);
                }
            } else if cycle == person.next_status_change {
                person.update_disease_status(cycle, &self.sim_parms);
            }
        }
    }
}

// cell
#[derive(Debug)]
struct Cell<'a> {
    id: usize,
    populace: Vec<Person>,
    // neighbours: Vec<Cell<'a>>,
    resident_pop: usize,     // total residents  including away
    at_home_pop: usize,      // residents currently at home
    visitor_pop: usize,      // visitors
    testing_capacity: usize, // people per cycle
}

impl<'a> Cell<'a> {
    fn new(id: usize) -> Cell<'a> {
        let populace: Vec<Person> = Vec::new();
        // let neighbours: Vec<Cell> = Vec::new();
        // let at_home_pop = 0;
        // let testing_capacity = 0;
        Cell {
            id: id,
            populace: populace,
            resident_pop: 0,
            at_home_pop: 0,
            visitor_pop: 0,
            // neighbours: neighbours,
            testing_capacity: 0,
        }
    }

    fn resident_pop(&self) -> usize {
        self.resident_pop
    }
    fn visitor_pop(&self) -> usize {
        self.visitor_pop
    }
    fn at_home_pop(&self) -> usize {
        self.at_home_pop
    }
    fn away_pop(&self) -> usize {
        self.current_pop() - self.visitor_pop() - self.at_home_pop()
    }
    fn current_pop(&self) -> usize {
        self.populace.len()
    }

    // use this only to initialize cell pop
    fn add_resident(&self, p: person) {
        assert_eq!(self.id, person.home_cell);
        self.resident_pop += 1;
        self.add_person(p);
    }
    fn add_person(&self, p: Person) {
        match p.home_cell == self.id {
            true => self.at_home_pop += 1,
            false => self.visitor_pop += 1,
        }
        self.populace.push(p);
    }
    fn remove_person(&self, index: usize) -> Person {
        match p.home_cell == self.id {
            true => self.at_home_pop -= 1,
            false => self.visitor_pop -= 1,
        }
        self.populace.remove(index)
    }
}

// implements a 2D cell from a 1D index and row count
struct CellAddress {
    row_count: usize,
    col_count: usize,
    row_index: usize,
    col_index: usize,
}

impl CellAddress {
    fn new(index: usize, row_count: usize, col_count: usize) -> CellAddress {
        assert!(
            index < row_count * col_count,
            "Cell index {} >= max {}",
            index,
            row_count * col_count
        );
        CellAddress {
            row_count: row_count,
            col_count: col_count,
            row_index: index / col_count,
            col_index: index % col_count,
        }
    }

    // returns a vector of up to 8 <usize>.
    // these are the indices of the immediate neighbours of cell
    // cells on the edges and corners have fewer neighbours
    fn neighbours(&self) -> Vec<usize> {
        let mut neighbours: Vec<usize> = Vec::with_capacity(8);

        if self.row_index == 0 {
            // top row
            if self.col_index == 0 {
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index + 1) * self.row_count);
            } else if self.col_index < self.col_count - 1 {
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index + 1) * self.row_count);
            } else {
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
            }
        } else if self.col_index < self.row_count - 1 {
            if self.col_index == 0 {
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index + 1) * self.row_count);
            } else if self.col_index < self.col_count - 1 {
                neighbours.push(self.row_index - 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index + 1) * self.row_count);
            } else {
                neighbours.push(self.row_index - 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + 1 + (self.col_index) * self.row_count);
            }
        } else {
            if self.col_index == 0 {
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
            } else if self.col_index < self.col_count - 1 {
                neighbours.push(self.row_index - 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index + 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index + (self.col_index + 1) * self.row_count);
            } else {
                neighbours.push(self.row_index - 1 + (self.col_index - 1) * self.row_count);
                neighbours.push(self.row_index - 1 + (self.col_index) * self.row_count);
                neighbours.push(self.row_index + (self.col_index - 1) * self.row_count);
            }
        }
        neighbours
    }
}

// Person ---------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Person {
    pub id: usize,
    pub mobility: f64,
    pub exposure: f64,
    pub compliability: f64,
    pub immunity: f64,
    // Vulnerability: an integer from 0 to 4 describing the worst disease path a person will follow if infected
    // See the update disease status function
    pub vulnerability: usize,
    pub home_cell: usize,
    pub current_cell: usize,
    pub disease_status: DiseaseStatus,
    pub next_status_change: usize, //cycle on which on next change will occur
    pub event_log: Vec<PersonLogEntry>,
    pub last_test: Option<(bool, usize)>, // test result, test cycle
}

impl Person {
    fn new(id: usize, parms: &SimParms, address: usize) -> Person {
        let mut rng = thread_rng(); // prep random # generator (I think)
        let mobility = parms.mobility.sample(&mut rng);
        let exposure = parms.exposure.sample(&mut rng);
        let compliability = parms.compliability.sample(&mut rng);
        let vulnerability = parms.vulnerability.sample(&mut rng);
        let immunity = 0.0;
        let mut event_log: Vec<PersonLogEntry> = Vec::new();
        event_log.push(PersonLogEntry {
            id: id,
            cycle: 0,
            current_cell: address,
            event: PersonEvent::Create(mobility, exposure, compliability, immunity, vulnerability),
        });
        Person {
            id: id,
            mobility: mobility,
            exposure: exposure,
            compliability: compliability,
            vulnerability: vulnerability,
            immunity: immunity,
            home_cell: address,
            current_cell: address,
            disease_status: DiseaseStatus::Well,
            next_status_change: 0,
            event_log: event_log,
            last_test: None,
        }
    }

    fn relocate(&mut self, cycle: usize, new_cell_index: usize) {
        self.event_log.push(PersonLogEntry {
            id: self.id,
            cycle: cycle,
            current_cell: self.current_cell,
            event: PersonEvent::Move(new_cell_index),
        });
        self.current_cell = new_cell_index;
    }

    // update disease status
    // each person has a randomly assigned vulnerability level which predetermines the path in disease progression
    // there are 5 vulnerability levels can be 0,1,2,3,4
    // using the code W-Well, I-Infected, S-Symptomatic, H-Hospitalized, D-Dead the paths are
    //    0: W>I>W
    //    1: W>I>S>W
    //    2: W>I>S>H>W
    //    3: W>I>S>D
    //    4: W>I>S>H>D
    // this function is called when a person gets infected or when they hit a cycle when they are "due" for
    // their next status change
    // the routine sets their new disease state, the (randomly determined) date on which they will move to the next state
    // and their new immunity level.
    fn update_disease_status(&mut self, cycle: usize, parms: &SimParms) -> DiseaseStatus {
        let mut rng = rand::thread_rng();

        // create a 3 element tuple with 0-new disease status
        //                               1-cycles to next status change
        //                               2-new immunity level
        let new_status = match self.disease_status {
            DiseaseStatus::Well => (
                DiseaseStatus::Infected,
                cycle + (parms.days_infected.sample(&mut rng)) as usize,
                None,
            ),
            DiseaseStatus::Infected => {
                if self.vulnerability == 0 {
                    (DiseaseStatus::Well, 0, Some(1.0))
                } else {
                    (
                        DiseaseStatus::Symptomatic,
                        cycle + (parms.days_symptomatic.sample(&mut rng)) as usize,
                        None,
                    )
                }
            }
            DiseaseStatus::Symptomatic => {
                if self.vulnerability == 1 {
                    (DiseaseStatus::Well, 0, Some(1.0))
                } else if self.vulnerability == 2 || self.vulnerability == 4 {
                    (
                        DiseaseStatus::Hospitalized,
                        cycle + (parms.days_hospitalized.sample(&mut rng)) as usize,
                        None,
                    )
                } else {
                    (DiseaseStatus::Dead, 0, Some(0.0))
                }
            }

            DiseaseStatus::Hospitalized => {
                if self.vulnerability == 2 {
                    (DiseaseStatus::Well, 0, Some(1.0))
                } else {
                    (DiseaseStatus::Dead, 0, Some(0.0))
                }
            }
            DiseaseStatus::Dead => (DiseaseStatus::Dead, 0, Some(0.0)),
        };
        self.event_log.push(PersonLogEntry {
            id: self.id,
            cycle: cycle,
            current_cell: self.current_cell,
            event: PersonEvent::DiseaseStatusChange(new_status.0),
        });

        self.disease_status = new_status.0;
        self.next_status_change = new_status.1;
        match new_status.2 {
            Some(immunity) => self.immunity = immunity,
            None => (),
        }
        new_status.0
    }

    // test to see if infected
    // sim parameters determine test reliability
    //      result may be accurate or
    //      false positive or
    //      false negative
    fn infection_test(&mut self, cycle: usize, parms: &SimParms) {
        // naive version - perfect test TODO #4 - put in errors
        let is_infected: bool = if self.disease_status == DiseaseStatus::Well {
            true
        } else {
            false
        };

        self.last_test = Some((is_infected, cycle));
        self.event_log.push(PersonLogEntry {
            id: self.id,
            cycle: cycle,
            current_cell: self.current_cell,
            event: PersonEvent::Test(is_infected),
        });
    }
}

#[derive(Debug, Copy, Clone, Serialize)]
pub enum PersonEvent {
    Create(f64, f64, f64, f64, usize), // mobility, exposure, compliability, immunity, vulnerability
    Move(usize),                       // to_cell index
    DiseaseStatusChange(DiseaseStatus), // new disease status
    Test(bool),                        // test with status result
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct PersonLogEntry {
    pub id: usize,
    pub cycle: usize,
    pub current_cell: usize,
    pub event: PersonEvent,
}

// Disease -------------------------------------------------------------------------------------------
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub enum DiseaseStatus {
    Well,
    Infected,
    Symptomatic,
    Hospitalized,
    Dead,
}
impl fmt::Display for DiseaseStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// Parameters --------------------------------------------------------------------------------------
pub struct WorldParms {
    pub rows: usize,
    pub cols: usize,
    pub people_count: usize,
    pub sim_parms: SimParms,
}

impl WorldParms {
    pub fn new(rows: usize, cols: usize, people_count: usize, sim_parms: SimParms) -> WorldParms {
        WorldParms {
            rows: rows,
            cols: cols,
            people_count: people_count,
            sim_parms: sim_parms,
        }
    }
}

// Random parameters are Beta distribtions which run between 0 and 1
// Some are also given a multipler to scale the distribution up and a scaler to shift and stretch the distribution
#[derive(Debug)]
pub struct SimParms {
    mobility: Beta<f64>,      // person's tendancy to move around
    exposure: Beta<f64>,      // person's tendancy to transfer/receive virus
    compliability: Beta<f64>, // person's compliancy to social measures
    immunity_loss: Beta<f64>, // fraction of immunity lost per day
    // Distribution of population vulnerability to the virus
    // It is a 5 element vector
    vulnerability: rand::distributions::WeightedIndex<f64>,
    days_infected: Pert<f64>,     // days in infected state
    days_symptomatic: Pert<f64>,  // days in symptomatic state
    days_hospitalized: Pert<f64>, // days in hospitalized state
}

impl SimParms {
    pub fn new(
        mobility: (f64, usize),             //mean, sample_population
        exposure: (f64, usize),             //mean, sample_population
        compliability: (f64, usize),        //mean, sample_population
        immunity_loss: (f64, usize),        //mean, sample_population
        vulnerability: &Vec<f64>,           //I->W, I->S, S->W, S->H, S->D, H->D TODO
        days_infected: (f64, f64, f64),     //min, max, mode
        days_symptomatic: (f64, f64, f64),  //min, max, mode
        days_hospitalized: (f64, f64, f64), //min, max, mode
    ) -> SimParms {
        assert!(
            vulnerability.len() >= 5,
            "Vulnerability must have five values.  It has {}",
            vulnerability.len()
        );

        // convert mean, sample_population to distriburtion parameters
        let mobility_ab = SimParms::beta_mv_to_ab(mobility.0, mobility.1);
        // println!("Mobility: alpha {} - beta {}", mobility_ab.0, mobility_ab.1);
        let exposure_ab = SimParms::beta_mv_to_ab(exposure.0, exposure.1);
        // println!("Exposure: alpha {} - beta {}", exposure_ab.0, exposure_ab.1);
        let compliability_ab = SimParms::beta_mv_to_ab(compliability.0, compliability.1);
        // println!(
        //     "Compliability: alpha {} - beta {}",
        //     compliability_ab.0, compliability_ab.1
        // );
        let immunity_loss_ab = SimParms::beta_mv_to_ab(immunity_loss.0, immunity_loss.1);
        // println!(
        //     "Immunity loss: alpha {} - beta {}",
        //     immunity_loss_ab.0, immunity_loss_ab.1
        // );

        SimParms {
            mobility: Beta::new(mobility_ab.0, mobility_ab.1).expect("Mobility beta"),
            exposure: Beta::new(exposure_ab.0, exposure_ab.1).expect("Exposure beta"),
            compliability: Beta::new(compliability_ab.0, compliability_ab.1)
                .expect("Compliability beta"),
            immunity_loss: Beta::new(immunity_loss_ab.0, immunity_loss_ab.1)
                .expect("Immunity loss beta"),
            vulnerability: rand::distributions::WeightedIndex::new(vulnerability)
                .expect("Vulnerability weighted"),
            days_infected: Pert::new(days_infected.0, days_infected.1, days_infected.2)
                .expect("Days infected PERT"),
            days_symptomatic: Pert::new(days_symptomatic.0, days_symptomatic.1, days_symptomatic.2)
                .expect("Days symptomatic PERT"),
            days_hospitalized: Pert::new(
                days_hospitalized.0,
                days_hospitalized.1,
                days_hospitalized.2,
            )
            .expect("Days hospitalized PERT"),
        }
    }

    // pub fn new_default() -> SimParms {
    //     SimParms {
    //         mobility: Beta::new(1.0, 19.0).unwrap(),
    //         exposure: Beta::new(2.0, 5.0).unwrap(),
    //         compliability: Beta::new(2.0, 5.0).unwrap(),
    //         immunity_loss: Beta::new(1.0, 1000.0).unwrap(),
    //         vulnerability: rand::distributions::WeightedIndex::new(vec![0.39, 0.50, 0.15, 0.01])
    //             .unwrap(),
    //         days_infected: Pert::new(1.0, 20.0, 7.0).unwrap(),
    //         days_symptomatic: Pert::new(1.0, 14.0, 7.0).unwrap(),
    //         days_hospitalized: Pert::new(1.0, 21.0, 10.0).unwrap(),
    //     }
    // }

    // convert mean and sample population to alpha beta
    fn beta_mv_to_ab(mean: f64, sample_population: usize) -> (f64, f64) {
        let alpha = mean * sample_population as f64;
        let beta = (1.0 - mean) * sample_population as f64;
        (alpha, beta)
    }
}

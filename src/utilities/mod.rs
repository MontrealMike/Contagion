/////////////////////////////////////////////////////////////////////////////////////
//
// Contagion model
//
// utilities module
//
// various functions thta can be of use across the app
//
////////////////////////////////////////////////////////////////////////////////////


use rand::distributions::Distribution;
use rand::prelude::*;


// A selection of distributions
enum DistParms {
    StandardNormal(u32, u32),   // normally dist between min, max
    Normal(f32, f32),           // mean, sdev
    Uniform(i32, i32),          // range first, range last
    Weighted(Vec<f32>),         // vector of weightings
    LogNormal(f32, f32),        // mean, sdev
}


// generate a distribution based on DistParms argument
// TODO - I suspect this is highly inefficient code - there must be a better way
fn get_distribution(dp: DistParms) -> f64 {

}

use bevy::prelude::*;

fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_startup_system(add_resources.system())
        .add_system(show_resource.system())
        .add_system(hello_world.system())
        .run();
}

fn hello_world() {
    println!("Hello world!")
}

struct Resource(f64);

struct Name(String);

fn add_resources(mut commands: Commands) {
    commands
        .spawn((Resource(80.0), Name("Mana".to_string())))
        .spawn((Resource(0.0), Name("Rage".to_string())));
}

fn show_resource(_resource: &Resource, _name: &Name) {
    println!("Current {}: {}", _name.0, _resource.0);
}
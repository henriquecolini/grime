use std::collections::HashMap;

use super::Location;

#[derive(Debug, Clone, Copy)]
enum ItemStrength {
	Weak,
	Strong,
}

#[derive(Debug, Clone)]
struct Item<T> {
	st: ItemStrength,
	loc: Location,
	item: T,
}

#[derive(Debug, Clone)]
pub struct ItemList<T> {
	items: HashMap<String, Item<T>>,
}

fn weak<T>(loc: Location, item: T) -> Item<T> {
	Item {
		st: ItemStrength::Weak,
		loc,
		item,
	}
}

fn strong<T>(loc: Location, item: T) -> Item<T> {
	Item {
		st: ItemStrength::Strong,
		loc,
		item,
	}
}

impl<T> ItemList<T> {
	pub fn new() -> Self {
		Self {
			items: HashMap::new(),
		}
	}

	pub fn weak_insert(&mut self, key: String, loc: Location, item: T) -> Result<(), Location> {
		let presence = {
			let curr = self.items.get(&key);
			curr.map(|c| (c.st))
		};
		match presence {
			Some(ItemStrength::Strong) => Ok(()),
			Some(ItemStrength::Weak) => Err(self.items.get(&key).unwrap().loc.clone()),
			None => {
				self.items.insert(key.to_owned(), weak(loc, item));
				Ok(())
			}
		}
	}

	pub fn insert(&mut self, key: String, loc: Location, item: T) {
		self.items.insert(key.to_owned(), strong(loc, item));
	}

	pub fn get_strong(&self, key: &str) -> Option<(Location, &T)> {
		self.items.get(key).and_then(|i| match i.st {
			ItemStrength::Weak => None,
			ItemStrength::Strong => Some((i.loc.clone(), &i.item)),
		})
	}

	pub fn get(&self, key: &str) -> Option<&T> {
		self.items.get(key).map(|i| &i.item)
	}

	pub fn keys(&self) -> impl Iterator<Item = &String> {
		self.items.keys()
	}

	pub fn iter_strong(&self) -> impl Iterator<Item = (&String, &T)> {
		self.items.iter().filter_map(|i| match i.1.st {
			ItemStrength::Weak => None,
			ItemStrength::Strong => Some((i.0, &i.1.item)),
		})
	}
}

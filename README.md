# retro_restaurant

![Retro Restaurant](https://westsidetoday-enki-v2.s3.amazonaws.com/wp-content/uploads/2017/08/Picture-Device-Independent-Bitmap-2.jpg)

> India Hall originally included one of the LA’s first vegetarian restaurants (serving a popular “Mushroom Burger”), which ceased operation in 1969 after nearly 20 successful years

```haskell

$ cabal run
Starting Restaurant Simulation..
Kitchen starting with 2 order takers, 3 cooks, and 3 waitresses
Patron 1, Placed new order 1 (menu item ChickenCombo)
Patron 2, Placed new order 2 (menu item GardenVariety)
Patron 3, Placed new order 3 (menu item ChickenCombo)
Patron 4, Placed new order 4 (menu item MahiMahi)
Patron 5, Placed new order 5 (menu item MahiMahi)
 Received Order 1
 Received Order 2
 Received Order 3
 Received Order 4
 Received Order 5
   -> Dequeued job from tier priority q: VIP, Id: 151, Order 3, Prio: 1, Name: "ChickenCombo Grill Chicken for Patron 3", Reqs: []
 Cook 1, preparing job 151
   -> Dequeued job from tier priority q: VIP, Id: 154, Order 3, Prio: 1, Name: "ChickenCombo Fry Side Dish for Patron 3", Reqs: []
 Cook 2, preparing job 154
   -> Dequeued job from tier priority q: VIP, Id: 251, Order 5, Prio: 1, Name: "MahiMahi Grill Fish for Patron 5", Reqs: []
 Cook 3, preparing job 251
   -> Dequeued job from tier priority q: VIP, Id: 152, Order 3, Prio: 2, Name: "ChickenCombo Toast Bun for Patron 3", Reqs: []
 Cook 3, preparing job 152
   -> Dequeued job from tier priority q: VIP, Id: 253, Order 5, Prio: 2, Name: "MahiMahi Make Sauce for Patron 5", Reqs: []
 Cook 1, preparing job 253
   -> Dequeued job from tier priority q: VIP, Id: 252, Order 5, Prio: 2, Name: "MahiMahi Toast Bun for Patron 5", Reqs: []
 Cook 2, preparing job 252
 ! Cook 3, made a mistake on job152
   -> Dequeued job from tier priority q: VIP, Id: 152, Order 3, Prio: 2, Name: "ChickenCombo Toast Bun for Patron 3", Reqs: []
 Cook 3, preparing job 152
   -> Dequeued job from tier priority q: Car, Id: 51, Order 1, Prio: 1, Name: "ChickenCombo Grill Chicken for Patron 1", Reqs: []
 Cook 1, preparing job 51
   -> Dequeued job from tier priority q: VIP, Id: 254, Order 5, Prio: 2, Name: "MahiMahi Make Burger for Patron 5", Reqs: [251,252,253]
 Cook 2, preparing job 254
 ! Cook 3, made a mistake on job152
 ! Cook 3, made too many mistakes ~ taking an unpaid break
 ! Cook 1, made a mistake on job51
   -> Dequeued job from tier priority q: VIP, Id: 152, Order 3, Prio: 2, Name: "ChickenCombo Toast Bun for Patron 3", Reqs: []
 Cook 1, preparing job 152
   -> Dequeued job from tier priority q: Car, Id: 54, Order 1, Prio: 1, Name: "ChickenCombo Fry Side Dish for Patron 1", Reqs: []
 Cook 3, preparing job 54
+ Cook 3, restarted and given the chance to cook again
✓ Food cooked for order 5 and ready to eat (thanks Cook 2) assistant preparing drinks
   -> Dequeued job from tier priority q: Car, Id: 51, Order 1, Prio: 1, Name: "ChickenCombo Grill Chicken for Patron 1", Reqs: []
 Cook 2, preparing job 51
✓ Drink prepared for order 5
   -> Dequeued job from tier priority q: VIP, Id: 153, Order 3, Prio: 3, Name: "ChickenCombo Make Burger for Patron 3", Reqs: [151,152]
 Cook 1, preparing job 153
   -> Dequeued job from tier priority q: Car, Id: 101, Order 2, Prio: 1, Name: "GardenVariety Prepare Mushrooms for Patron 2", Reqs: []
 Cook 3, preparing job 101
   -> Dequeued job from tier priority q: Car, Id: 52, Order 1, Prio: 2, Name: "ChickenCombo Toast Bun for Patron 1", Reqs: []
 Cook 2, preparing job 52
✓ Food cooked for order 3 and ready to eat (thanks Cook 1) assistant preparing drinks
   -> Dequeued job from tier priority q: Car, Id: 201, Order 4, Prio: 1, Name: "MahiMahi Grill Fish for Patron 4", Reqs: []
 Cook 1, preparing job 201
✓ Drink prepared for order 3
 ! Cook 3, made a mistake on job101
   -> Dequeued job from tier priority q: Car, Id: 101, Order 2, Prio: 1, Name: "GardenVariety Prepare Mushrooms for Patron 2", Reqs: []
 Cook 3, preparing job 101
   -> Dequeued job from tier priority q: Car, Id: 202, Order 4, Prio: 2, Name: "MahiMahi Toast Bun for Patron 4", Reqs: []
 Cook 2, preparing job 202
   -> Dequeued job from tier priority q: Car, Id: 203, Order 4, Prio: 2, Name: "MahiMahi Make Sauce for Patron 4", Reqs: []
 Cook 1, preparing job 203
   -> Dequeued job from tier priority q: Car, Id: 106, Order 2, Prio: 2, Name: "GardenVariety Fry Side Dish for Patron 2", Reqs: []
 Cook 3, preparing job 106
 ! Cook 2, made a mistake on job202
   -> Dequeued job from tier priority q: Car, Id: 202, Order 4, Prio: 2, Name: "MahiMahi Toast Bun for Patron 4", Reqs: []
 Cook 2, preparing job 202

 ~~ Waitress 1, delivered order 5, priority VIP for MahiMahi to hungry Patron 5

   -> Dequeued job from tier priority q: Car, Id: 103, Order 2, Prio: 2, Name: "GardenVariety Toast Bun for Patron 2", Reqs: []
 Cook 1, preparing job 103

 ~~ Waitress 2, delivered order 3, priority VIP for ChickenCombo to hungry Patron 3

   -> Dequeued job from tier priority q: Car, Id: 102, Order 2, Prio: 2, Name: "GardenVariety Grill Mushrooms for Patron 2", Reqs: [101]
 Cook 3, preparing job 102
   -> Dequeued job from tier priority q: Car, Id: 204, Order 4, Prio: 2, Name: "MahiMahi Make Burger for Patron 4", Reqs: [201,202,203]
 Cook 2, preparing job 204
   -> Dequeued job from tier priority q: Car, Id: 53, Order 1, Prio: 3, Name: "ChickenCombo Make Burger for Patron 1", Reqs: [51,52]
 Cook 1, preparing job 53
   -> Dequeued job from tier priority q: Car, Id: 104, Order 2, Prio: 2, Name: "GardenVariety Make Burger for Patron 2", Reqs: [101,102,103]
 Cook 3, preparing job 104
✓ Food cooked for order 4 and ready to eat (thanks Cook 2) assistant preparing drinks
   -> Dequeued job from tier priority q: Car, Id: 105, Order 2, Prio: 3, Name: "GardenVariety Make Salad for Patron 2", Reqs: []
 Cook 2, preparing job 105
✓ Food cooked for order 1 and ready to eat (thanks Cook 1) assistant preparing drinks
✓ Drink prepared for order 4
✓ Drink prepared for order 1
 ! Cook 2, made a mistake on job105
 ! Cook 2, made too many mistakes ~ taking an unpaid break
   -> Dequeued job from tier priority q: Car, Id: 105, Order 2, Prio: 3, Name: "GardenVariety Make Salad for Patron 2", Reqs: []
 Cook 1, preparing job 105
+ Cook 2, restarted and given the chance to cook again

 ~~ Waitress 3, delivered order 4, priority Car for MahiMahi to hungry Patron 4

✓ Food cooked for order 2 and ready to eat (thanks Cook 1) assistant preparing drinks
✓ Drink prepared for order 2

 ~~ Waitress 1, delivered order 1, priority Car for ChickenCombo to hungry Patron 1


 ~~ Waitress 2, delivered order 2, priority Car for GardenVariety to hungry Patron 2


<Metric Summary>
 Orders placed: 5
 Cook Job Failures: 6
 Cook Job Successes: 22
 Jobs Cooked by Cook: Cook 1: 9, Cook 2: 7, Cook 3: 6
 Cooked Orders: 5
 Delivered Orders: 5
 Restaurant simulation ended...
```

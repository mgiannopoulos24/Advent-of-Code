So for this level have you have to do is to paste your input to [this website](https://mazegame.org/adventofcode/2019/day25/index.html). After you paste it in the input where it says `Paste you Intcode input here`, press `Load IntCode`. This will generate the map according to your exact input.  
Now just use the arrow keys, move around and collect every item except these 4:

- escape pod
- photons
- infinite loop
- molten lava

If you take one of these 4 items you will lose and you will have to restart.

Since the items for each map are different besides the dangerous ones, here is a list of my items:
- mutex
- spool of cat6
- hypercube
- sand
- astronaut ice cream
- mouse
- antenna
- boulder

There's also an item that when taken you cant move so dont use it. That item is  the `giant electromagnet`. 

So after moving around in the map you will see in the end something like this:

<p align="center">

<img src="https://i.imgur.com/3iX3Mge.png">

</p>

Now after you collect all the useful items find a block that says **Security Checkpoint**. Now try to enter but it wont let you. So press the `Bruteforce` command and it will bruteforce it by dropping and picking up items rapidly. Finally you will get this message:

<p align="center">

<img src="https://i.imgur.com/ykbMIFn.png">

</p>

That is the password to complete the level.
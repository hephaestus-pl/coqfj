#FeatherWeight Java 
Featherweight Java is an implementation of [this paper](https://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf)

The aim of this work is to run correct FJ programs.

To get started first import the project with 
```
git clone https://github.com/hephaestus-pl/hffj
```

Navigate to the project folder 
``` cd hffj/src```

Then make the project with 
```
make fj
```
Now you are ready to run a program like:

```
class Int extends Object{
    Int(){super();}

    Int add(Int rhs){
	return rhs.add(this);
    }
}

class O extends Int{
    O(){super();}

    Int add(Int rhs){
        return rhs;
    }
}

class S extends Int{
    Int num;

    S(Int num){
        super();
        this.num=num;
    }

    Int add(Int rhs){
        return this.num.add(new S(rhs));
    }
    
}
new S(new S(new O())).add(new S(new O()))
```
Save your program in a file then run

```
FJ/Main file
```
You should see it working as expected!

Notice that you can also run in the silent mode with the [-s] directive

```
FJ/Main -s file
```

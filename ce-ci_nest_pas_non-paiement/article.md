No-one likes non-payment
=====


Surprising, there are 2 or three things you can achieve with code that is never run.
* Preventing that -wall -werror build from being created
   "no return from non-void function"
* Avoiding having to test that complex code path
  it *looks* like all cases result in an action
* A super simple way to dump the environment
  when you just can't look up the documentation



TODO(PMM)

First version
----


```C++

string output;
int switches[] = { 1, 2, 3, 4 };

for (int i : switches) {
	oopsN(i, output);
	cout << output << "\n";
}

```

Various ways of omitting to make an update


```C++


switch

void oops1(int condition, string& output) {
	switch (condition) {

	case 1:
		output = "1";
		break;
	case 2:
		output = "2";
		break;
	case 3:
		output = "3";
		break;
		defaut: output = "I have no clue";

	}

}

void oops2(int condition, string& output) {
	output = "";
	switch (condition) {

	case 1:
		output = "1";
		break;
	case 2:
		output = "2";
		break;
	case 3:
		output = "3";
		break;
		defaut: output = "I have no clue";

	}

}

void oops3(int condition, string& output) {
	switch (condition) {

	case 1:
		output = "1";
		break;
	case 2:
		output = "2";
		break;
	case 3:
		output = "3";
		break;
	default:
		output = "I have no clue";

	}

}


```


the output is....

    '1'
    '2'
    '3'
    '3'
    '1'
    '2'
    '3'
    ''
    '1'
    '2'
    '3'
    'I have no clue'



Upping the ante
---

Some people like multiple points of return ...


```C++

string output;
int switches[] = { 1, 2, 3, 4 };

for (int i : switches) {
	
	cout << oopsN(i) << "\n";
}

```



```C++


string oops4(int condition) {
	switch (condition) {

	case 1:
		return "1";
	case 2:
		return "2";
	case 3:
		return "3";
	defaut:
		return "I have no clue";

	}

}

string oops5(int condition) {
	switch (condition) {

	case 1:
		return "1";
	case 2:
		return "2";
	case 3:
		return "3";
	defaut:
		return "I have no clue";

	}

  return "";
}

string oops6(int condition) {
	switch (condition) {

	case 1:
		return "1";
	case 2:
		return "2";
	case 3:
		return "3";
	default:
		return "I have no clue";


	}

}


```


the output is:

![Image](protip.png)

So, now you know which OS type I ran this on, and have what looks like a decent chunk of that application's environment variables.


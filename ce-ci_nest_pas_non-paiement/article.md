No-one likes non-payment
=====




First version
----

Various ways of omitting to make an update


```C


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

```C


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




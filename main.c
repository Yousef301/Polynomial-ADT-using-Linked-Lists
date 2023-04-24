// By Yousef Shamasneh
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define size 10

/*Most of my function pass pointer to pointer parameter.*/


//Representation of DLL node.
typedef struct Node {
    int cof;
    int exp;
    struct Node *next;
    struct Node *prev;
} node;

node *equations[size];
node *results[size];
int global_cnt = 0;
int res_cnt = 0;

//This function is used to insert a node at the end of DLL.
void InsertEnd(node **list, int cof, int exp) {
    node *temp = (node *) malloc(sizeof(node));
    node *last = *list;
    temp->cof = cof;
    temp->exp = exp;
    temp->next = NULL;
    if (*list == NULL) {
        temp->prev = NULL;
        *list = temp;
        return;
    }
    while (last->next != NULL) {
        last = last->next;
    }
    last->next = temp;
    temp->prev = last;
}

//Function to delete that last node from the DLL.
void deleteLastNode(node **equation) {
    node *temp = *equation;
    while (temp->next != NULL) {
        temp = temp->next;
    }
    temp->prev->next = NULL;
    free(temp->next);
}

//This function will use to take each line from the inputs file (equation by equation) and then save the coefficients and
//exponents for each equation in the same DLL but in different nodes, by following a specific pattern.
void SaveInDLL(const char *line) {
    char coef[3], exp[2], sign = '+';
    int coef_cnt = 0, exp_cnt = 0, i = 0;
    bool ready = false;
    while (line[i] != '\n') {
        if (ready) { // saving coefficient and exponents inside a node when they are ready to save.
            coef[coef_cnt] = '\0';
            exp[exp_cnt] = '\0';
            int x, y;
            x = atoi(coef);
            y = atoi(exp);
            if (sign == '-')
                x *= -1;

            if (x == 0)
                x = 1;
            InsertEnd(&equations[global_cnt], x, y);
            coef_cnt = 0;
            exp_cnt = 0;
            ready = false;
        }
        if (line[i] == '-' || line[i] == '+') {
            sign = line[i];
            i++;
        }
        if (isdigit(line[i])) {
            while (isdigit(line[i])) {
                coef[coef_cnt] = line[i];
                coef_cnt++;
                i++;
            }
            ready = true;
        }
        if (isalpha(
                line[i])) { //when it's time to read the variable there are different cases that we need to pay attention to, and this if statements will handle with it.
            if ((line[i - 1] == '-' || line[i - 1] == '+') && line[i + 1] != '^' && i > 1) {
                exp[exp_cnt] = '1';
                exp_cnt++;
                coef[coef_cnt] = '1';
                coef_cnt++;
                i++;
                ready = true;
            } else if ((line[i - 1] == '-' || line[i - 1] == '+') && line[i + 1] == '^' && i > 1) {
                i = i + 2;
                while (isdigit(line[i])) {
                    exp[exp_cnt] = line[i];
                    exp_cnt++;
                    i++;
                }
                coef[coef_cnt] = '1';
                coef_cnt++;
                ready = true;
            } else if (line[i + 1] != '^') {
                exp[exp_cnt] = '1';
                exp_cnt++;
                i++;
                ready = true;
            } else {
                i = i + 2;
                while (isdigit(line[i])) {
                    exp[exp_cnt] = line[i];
                    exp_cnt++;
                    i++;
                }
                ready = true;
            }
        }
    }
    //Handling with the last node.
    coef[coef_cnt] = '\0';
    exp[exp_cnt] = '\0';
    int x, y;
    x = atoi(coef);
    y = atoi(exp);
    if (sign == '-') {
        x *= -1;
    }
    InsertEnd(&equations[global_cnt], x, y);
    global_cnt++;
}

//Printing equations from DLL using cx^e pattern and sign if necessary.
void PrintList(node *list) {
    node *temp = list;
    while (temp != NULL) {
        setbuf(stdout, 0);
        printf("%dx^%d", temp->cof, temp->exp);
        if (temp->next != NULL && temp->next->cof >= 0) {
            printf("+");
        }
        temp = temp->next;
    }
    printf("\n");
}

//This function is used to delete spaces from the line (if there are spaces) before reading the information we need from it.
void removeSpaces(char *line) {
    const char *edited = line;
    do {
        while (*edited == ' ') {
            ++edited;
        }
    } while (*line++ = *edited++);
}

// sort the DLL in descending order depending on the exponent.
void sortList(node **list) {
    node *current = NULL, *index = NULL;
    int temp1, temp2;
    if (list == NULL) {
        return;
    } else {
        for (current = *list; current->next != NULL; current = current->next) {
            for (index = current->next; index != NULL; index = index->next) {
                if (current->exp < index->exp) {
                    temp1 = current->exp;
                    temp2 = current->cof;
                    current->exp = index->exp;
                    current->cof = index->cof;
                    index->exp = temp1;
                    index->cof = temp2;
                }
            }
        }
    }
}

// summing the nodes that has the same exponent.
void sumSame(node *eq, node **result) {
    int cnt = 0, p1_cnt = 0, p2_cnt = 0;
    bool not_saved = true;
    sortList(&eq);
    node *temp = eq;
    node *temp1 = eq;
    while (eq != NULL) {
        if (cnt > 0 && (eq->exp == eq->prev->exp)) {
            p1_cnt++;
        } else {
            while (temp != NULL) {
                if (eq->exp == temp->exp && (p1_cnt != p2_cnt)) {
                    int sum = eq->cof + eq->next->cof;
                    while (temp->exp == temp->next->exp) {
                        sum += temp->next->cof;
                        temp = temp->next;
                    }
                    InsertEnd(result, sum, eq->exp);
                    not_saved = false;
                }
                p2_cnt++;
                temp = temp->next;
            }
            if (not_saved) {
                InsertEnd(result, eq->cof, eq->exp);
            }
            p2_cnt = 0;
            p1_cnt++;
            temp = temp1;
        }
        cnt++;
        not_saved = true;
        eq = eq->next;
    }
}

//Summation operation on two equation.
void polyAdd(node *eq1, node *eq2, node **result) {
    InsertEnd(&eq1, 0, 0);
    InsertEnd(&eq2, 0, 0);
    sortList(&eq1);
    sortList(&eq2);
    while (eq1->next && eq2->next) {
        if (eq1->exp > eq2->exp) {
            InsertEnd(result, eq1->cof, eq1->exp);
            eq1 = eq1->next;
        } else if (eq1->exp < eq2->exp) {
            InsertEnd(result, eq2->cof, eq2->exp);
            eq2 = eq2->next;
        } else {
            InsertEnd(result, (eq1->cof + eq2->cof), eq1->exp);
            eq1 = eq1->next;
            eq2 = eq2->next;
        }
    }
    while (eq1->next || eq2->next) {
        if (eq1->next) {
            InsertEnd(result, eq1->cof, eq1->exp);
            eq1 = eq1->next;
        }
        if (eq2->next) {
            InsertEnd(result, eq2->cof, eq2->exp);
            eq2 = eq2->next;
        }
    }
}

//Subtraction operation on two equation.
void polySub(node *eq1, node *eq2, node **result) {
    InsertEnd(&eq1, 0, 0);
    InsertEnd(&eq2, 0, 0);
    sortList(&eq1);
    sortList(&eq2);
    while (eq1->next && eq2->next) {
        if (eq1->exp > eq2->exp) {
            InsertEnd(result, eq1->cof, eq1->exp);
            eq1 = eq1->next;
        } else if (eq1->exp < eq2->exp) {
            InsertEnd(result, eq2->cof * -1, eq2->exp);
            eq2 = eq2->next;
        } else {
            InsertEnd(result, (eq1->cof - eq2->cof), eq1->exp);
            eq1 = eq1->next;
            eq2 = eq2->next;
        }
    }
    while (eq1->next || eq2->next) {
        if (eq1->next) {
            InsertEnd(result, eq1->cof, eq1->exp);
            eq1 = eq1->next;
        }
        if (eq2->next) {
            InsertEnd(result, eq2->cof * -1, eq2->exp);
            eq2 = eq2->next;
        }
    }

}

//Multiplication operation on two equation.
void polyMulti(node *eq1, node *eq2, node **result) {
    node *temp = eq2;
    node *temp1;
    temp1 = NULL;
    while (eq1 != NULL) {
        while (temp != NULL) {
            InsertEnd(&temp1, (eq1->cof * temp->cof), (eq1->exp + temp->exp));
            temp = temp->next;
        }
        temp = eq2;
        eq1 = eq1->next;
    }
    sortList(&temp1);
    sumSame(temp1, result);
}

//this function will do any of the previous operations on all of the equations.
node *doAllEquations(char ope) {
    node *calculations[size];
    int l_cnt = 0;
    for (int i = 0; i < global_cnt; i++) {
        calculations[i] = NULL;
    }
    if (ope == '+') {
        for (int i = 0; i < global_cnt - 1; i++) {
            if (i == 0) {
                polyAdd(equations[i], equations[i + 1], &calculations[l_cnt]);
            } else {
                polyAdd(calculations[l_cnt], equations[i + 1], &calculations[l_cnt + 1]);
                l_cnt++;
            }
        }
        printf("The result of summing the equations is: ");
    } else if (ope == '-') {
        for (int i = 0; i < global_cnt - 1; i++) {
            if (i == 0) {
                polySub(equations[i], equations[i + 1], &calculations[l_cnt]);
            } else {
                polySub(calculations[l_cnt], equations[i + 1], &calculations[l_cnt + 1]);
                l_cnt++;
            }
        }
        printf("The result of subtracting the equations is: ");
    } else if (ope == '*') {
        for (int i = 0; i < global_cnt - 1; i++) {
            if (i == 0) {
                polyMulti(equations[i], equations[i + 1], &calculations[l_cnt]);
            } else {
                polyMulti(calculations[l_cnt], equations[i + 1], &calculations[l_cnt + 1]);
                l_cnt++;
            }
        }
        printf("The result of multiplying the equations is : ");
    }
    return calculations[l_cnt];
}

//substitute value in equation.
void subValue(node *eq) {
    int value;
    double sum = 0;
    printf("Enter a value to substitute into the function:");
    scanf(" %d", &value);
    while (eq != NULL) {
        sum += eq->cof * pow(value, eq->exp);
        eq = eq->next;
    }
    printf("\nThe result of substituting %d in the function is: %0.1f\n", value, sum);
}

//Write results inside a file.
void writeInFile(FILE *file) {
    for (int i = 0; i < 3; i++) {
        while (results[i] != NULL) {
            fprintf(file, "%dx^%d", results[i]->cof, results[i]->exp);
            if (results[i]->next != NULL && results[i]->next->cof >= 0) {
                fprintf(file, "+");
            }
            results[i] = results[i]->next;
        }
        fprintf(file, "\n");
    }
}

int main() {
    int ope;
    bool sum_flag = false, sub_flag = false, multi_flag = false; //This flags will prevent doing the same operation on the same equations more than one time.
    char line[50];
    for (int i = 0; i < size; i++) {
        equations[i] = NULL;
        results[i] = NULL;
    }
    FILE *read, *write;
    read = fopen("equations.txt", "r");
    write = fopen("results.txt", "a+");

    if (read == NULL) {
        printf("Check the file name please.\n");
        exit(1);
    } else {
        while (fgets(line, sizeof(line), read)) {
            strcat(line, "\n");
            if (line[0] != '\n') {
                removeSpaces(line);
                SaveInDLL(line);
            }
        }
        printf("\nEquations has been loaded into an array of DLL.\n");
        fclose(read);
    }

    while (1) {
        printf("\n1- Display equations.\n");
        printf("2- Sum the equations.\n");
        printf("3- Subtract the equations.\n");
        printf("4- Multiply the equations.\n");
        printf("5- Substitute a value.\n");
        printf("6- Save the equations into results.txt file.\n");
        printf("7- Exit.\n");
        printf("\nEnter your selection:");
        scanf("%d", &ope);
        switch (ope) {
            case 1:
                for (int i = 0; i < global_cnt; i++) {
                    PrintList(equations[i]);
                }
                break;

            case 2:
                if (sum_flag == false) {
                    results[res_cnt] = doAllEquations('+');
                    PrintList(results[res_cnt]);
                    for (int i = 0; i < global_cnt; i++) {
                        deleteLastNode(&equations[i]);
                    }
                    res_cnt++;
                    sum_flag = true;
                } else
                    printf("The addition was previously performed\n");
                break;
            case 3:
                if (sub_flag == false) {
                    results[res_cnt] = doAllEquations('-');
                    PrintList(results[res_cnt]);
                    for (int i = 0; i < global_cnt; i++) {
                        deleteLastNode(&equations[i]);
                    }
                    res_cnt++;
                    sub_flag = true;
                } else
                    printf("The subtraction was previously performed\n");
                break;
            case 4:
                if (multi_flag == false) {
                    results[res_cnt] = doAllEquations('*');
                    PrintList(results[res_cnt]);
                    res_cnt++;
                    multi_flag = true;
                } else
                    printf("The multiplication was previously performed\n");
                break;
            case 5:
                for (int i = 0; i < res_cnt; i++) {
                    printf("%d: ", i + 1);
                    PrintList(results[i]);
                }
                printf("\nChose an equation to substitute a value in:");
                int x;
                scanf("%d", &x);
                if (x == 1) {
                    subValue(results[0]);
                } else if (x == 2) {
                    subValue(results[1]);
                } else if (x == 3) {
                    subValue(results[2]);
                } else
                    printf("There are only three equations.");
                break;
            case 6:
                writeInFile(write);
                fclose(write);
                printf("\nResults has been saved successfully.\n");
                break;
            case 7:
                printf("\nThe program has been shut down.\n");
                exit(1);
            default:
                printf("\nInvalid input.\n");
        }
    }
}

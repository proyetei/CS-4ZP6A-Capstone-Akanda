# CS 4ZP6A CAPSTONE

## Content
- [Project Goal](#ssProjectGoal) 
- [Group Members](#ssGroupMembers) 
- [Demo](#ssDemo) 
- [Technology Stack](#ssTechnologyStack)
- [Available Test Cases](#ssAvailableCases)
- [How To Add Test Cases](#ssAddCases)
- [CI Workflows](#ssCIWorkflows)
- [CLI](#ssCLI)
- [Sources](#ssSources)

## Group Members <a id='ssGroupMembers'></a>
- Proyetei Akanda
- Esha Pisharody
- Zainab Abdulsada
- Grace Croome
- Marie Hollington
- Emma Willson


## Project Goal <a id='ssPojectGoal'></a>
The goal of this capstone project is to design and build an automated code generator that creates a series of tests of increasing size that will test the efficiency of modern
interactive proof assistants including Lean, Idris, Agda, and Coq. Testing will first be performed on these assistants as ‘language’, then we will move forth with proof testing.
The project will also include a command line interface enabling users to view the time and memory complexity and provide a link redirecting them to local webpages presenting visualizations of the measured data.

## Demo <a id='ssDemo'></a>
This is an example output for one of the test cases, please run GitHub Actions to run your own tests!
https://capstone-proyetei-proyeteis-projects.vercel.app/

## Technology Stack <a id='ssTechnologyStack'></a>
- Haskell for building the translators, grammars, and the automated code generator creating test cases for each of the 4 proof assistants (Idris, Agda, Lean, Rocq)
- Includes a CI/CD pipeline in GitHub Actions to run the tests using Go + Docker and generating JSON file with the benchmarking results
- Flask + Python for the backend
- HTML/Tailwind CSS/JavaScript for frontend
- Vercel for deployment

## Available Test Cases <a id='ssAvailableCases'></a>
<table>
    <tr>
        <th>Test Case</th>
        <th>Agda Example For Size N = 3</th>
    </tr>
    <tr>
        <td>LetExample (ID = 1)<br>A series of N nested let statements. </td>
        <td>
            <pre><code>n : Nat
n = let
    x1 = 1
 in
    let
    x2 = x1
 in
    let
    x3 = x2
 in
    x3</code></pre>
        </td>
    </tr>
    <tr>
        <td>LetAddExample (ID = 2)<br>A series of N nested let statements that define and use sequential variables based on previous definitions</td>
        <td>
            <pre><code>n : Nat
n = let
    x1 = 1
 in
    let
    x2 = x1  +  x1
 in
    let
    x3 = x2  +  x2
 in
    x3</code></pre>
        </td>
    </tr>
    <tr>
        <td>NestedFunction (ID = 3)<br>A series of N nested functions</td>
        <td>
            <pre><code>n : Nat
n = let
    f1 : Nat -> Nat
    f1 x1 = x1 + 1
    f2 : Nat -> Nat -> Nat
    f2 x1 x2 = 1 + x1 + x2
    f3 : Nat -> Nat -> Nat -> Nat
    f3 x1 x2 x3 = 1 + x1 + x2 + x3
    in f3 2 3 4 + f2 2 3 + f1 2</code></pre>
        </td>
    </tr>
    <tr>
        <td>DataSimpleDeclarations (ID = 4)<br>A specified number of simple datatype declarations</td>
        <td>
            <pre><code>data x3 : Set where
 y : Bool
data x2 : Set where
 y : Bool
data x1 : Set where
 y : Bool</code></pre>
        </td>
    </tr>
    <tr>
        <td>LongIdentifier (ID = 5)<br>Variable declaration with an identifier of a specified length</td>
        <td>
            <pre><code>xxx : Nat
xxx = 0</code></pre>
        </td>
    </tr>
    <tr>
        <td>Fields_DependentRecordModule (ID = 6)<br>A record declaration with N dependent fields</td>
        <td>
            <pre><code>record X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Vec Nat f1
        f3 : Vec Nat (suc f1)

example : X
example = Const 1 (1 ∷ []) (1 ∷ 1 ∷ [])</code></pre>
        </td>
    </tr>
    <tr>
        <td>ChainDef_DependentRecordModule (ID = 7)<br>A very long chain (N) of dependent record definitions</td>
        <td>
            <pre><code>record Record1 : Set where
    constructor Const1
    field
        f1 : Nat
record Record2 : Set where
    constructor Const2
    field
        f2 : Record1
record Record3 : Set where
    constructor Const3
    field
        f3 : Record2

example : Record3
example =  Const3 (Const2 (Const1 10) )</code></pre>
        </td>
    </tr>
    <tr>
        <td>Parameters_DependentRecordModule (ID = 8)<br>A record with N parameters</td>
        <td>
            <pre><code>record X (f1 : Nat) (f2 : Nat) (f3 : Nat) : Set where
    constructor Const
    field
        sums : Nat
        values : List Nat

example : X 1 2 3
example = Const (1 + 2 + 3)  (1 ∷ 2 ∷ 3 ∷ [])</code></pre>
        </td>
    </tr>
    <tr>
        <td>NewlineFile (ID = 9)<br>A file with N newlines</td>
        <td>
            <pre><code>

</code></pre>
        </td>
    </tr>
    <tr>
        <td>Fields_NonDependentRecordModule (ID = 10)</td>
        <td>
            <pre><code>record X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Nat
        f3 : Nat

example : X
example = Const 1 1 1</code></pre>
        </td>
    </tr>
    <tr>
        <td>ChainDefFields_NonDependentRecordModule (ID = 11)</td>
        <td>
            <pre><code>record Record1 : Set where
    constructor Const1
    field
        f1 : Nat
record Record2 : Set where
    constructor Const2
    field
        f2 : Nat
record Record3 : Set where
    constructor Const3
    field
        f3 : Nat

example : Record3
example = Const 1</code></pre>
        </td>
    </tr>
    <tr>
        <td>Constructors_Datatypes (ID = 12)</td>
        <td>
            <pre><code>data d : Set where
 c1 : d 
 c2 : d 
 c3 : d</code></pre>
        </td>
    </tr>
    <tr>
        <td>Parameters_Datatypes (ID = 13)</td>
        <td>
            <pre><code>data d (p1: Type)  (p2: Type)  (p3: Type) : Set where
 c : d p1 p2 p3</code></pre>
        </td>
    </tr>
    <tr>
        <td>FirstLast_VariableModule (ID = 14)<br>defines N variables, and uses both the first and last one in a declaration, N>=2</td>
        <td>
            <pre><code>x1 : Nat
x1 = 1
x2 : Nat
x2 = 2
x3 : Nat
x3 = 3
result : Nat
result = x1 + x3</code></pre>
        </td>
    </tr>
    <tr>
        <td>DeepDependency_VariableModule (ID = 15)<br>Defines a series of dependent variables, with 10 variables at each level of dependency, and then utilizes the innermost variables in a subsequent expression</td>
        <td> For N = 1
            <pre><code>x1L1 : Nat
x1L1 = 1
x1L2 : Nat
x1L2 = 2
x1L3 : Nat
x1L3 = 3
x1L4 : Nat
x1L4 = 4
x1L5 : Nat
x1L5 = 5
x1L6 : Nat
x1L6 = 6
x1L7 : Nat
x1L7 = 7
x1L8 : Nat
x1L8 = 8
x1L9 : Nat
x1L9 = 9
x1L10 : Nat
x1L10 = 10
result : Nat
result = 100 + x1L1 + x1L2 + x1L3 + x1L4 + x1L5 + x1L6 + x1L7 + x1L8 + x1L9 + x1L10</code></pre>
        </td>
    </tr>
    <tr>
        <td>DataImplicitIndices (ID = 16)<br>A simple datatype declaration with a specified number of indices, defined implicitly</td>
        <td>
            <pre><code>data D : Nat -> Nat -> Nat -> Set where
 C1 : {x3 x2 x1 : Nat} -> D x3 x2 x1</code></pre>
        </td>
    </tr>
    <tr>
        <td>SingleLongLine (ID = 17)<br>A file consisting of a single long line with N characters</td>
        <td>
            <pre><code>xxx</code></pre>
        </td>
    </tr>
    <tr>
        <td>ConstructorsParameters_Datatypes (ID = 18)<br>A single datatype where N represents the number of 'Type' parameters, all needed for N constructors</td>
        <td>
            <pre><code>data D (P1: Type)  (P2: Type)  (P3: Type) : Set where
 C1 : D P1 P2 P3 
 C2 : D P1 P2 P3 
 C3 : D P1 P2 P3</code></pre>
        </td>
    </tr>
    <tr>
        <td>IndicesConstructors_Datatypes (ID = 19)<br>A single datatype where N represents the number of indices, all needed for N constructors</td>
        <td>
            <pre><code>data D : Nat -> Nat -> Nat -> Set where
 C1 : {X1 : Nat} -> D 
 C2 : {X2 X1 : Nat} -> D 
 C3 : {X3 X2 X1 : Nat} -> D</code></pre>
        </td>
    </tr>
    <tr>
        <td>IndicesParameters_Datatypes (ID = 20)<br>A single datatype where N represents the number of 'Type' parameters as well as the number of indices</td>
        <td>
            <pre><code>data D (P1: Type)  (P2: Type)  (P3: Type) : Set where
 C : {X3 X2 X1 : Nat} -> D P1 P2 P3</code></pre>
        </td>
    </tr>
    <tr>
        <td>Pattern_Matching_Datatypes (ID = 21)<br>A function pattern matching on N constructors of a datatype</td>
        <td>
            <pre><code>data D : Set where
 C1 : D 
 C2 : D 
 C3 : D
N : Nat
N = let
    F: D -> Nat
        F C1 = 1 
        F C2 = 2 
        F C3 = 3 in
    F C3 + F C2 + F C1</code></pre>
        </td>
    </tr>

</table>

## How To Add Test Cases <a id='ssAddCases'></a>

### Writing Dynamic Test Cases in MHPG



### Extending MHPG

### Extending CI Workflows

### Extending CLI

## CI Workflows <a id='ssCIWorkflows'></a>

The CI implemented in github actions consists of 4 workflows (Build, Generate-List Testcases, Generate-Range Testcases, Tests). 

### Build Workflow
Allows users to build the CLI and the translator as well as creating and pushing the docker image. 

<img src="images/build.png">

### Generate-List Test Cases Workflow
Allows users to generate and type check a selected test case at specific sizes in Agda, Idris, Lean, and Rocq, and provides a URL where users can access the webpage with the time and memory results. 

<img src="images/generate-list.png">



### Generate-Range Test Cases Workflow
 Allows users to generate and type check a selected over a range of sizes with a linear, quadratic or log interval in Agda, Idris, Lean, and Rocq, and provides a URL where users can access the webpage with the time and memory results. 

<img src="images/generate-range.png">



### Tests Workflow
Allows users to test the translator for a selected testcase at a size between 1 and 20.

<img src="images/tests.png">

**Things to add:** 
- The tag of the dockerfile is based on the branch that is running the workflow, if it is the main branch the tag is set to "latest" otherwise it is set to the branch name
- required inputs for each workflow + descriptions
- Usage Instructions(i.e. Go to Actions Tab > Go to Build tab > Go to Run Workflow tab > Choose branch > Click Run Workflow) for each workflow???


## CLI <a id='ssCLI'></a>

### Local Installation Instructions
1. Install Docker https://docs.docker.com/engine/install/
2. Pull Docker image `docker pull mhpgeez/mhpg`
3. For CLI instructions run the Docker container using the following command: `docker run -it --rm -p "5001:5001" mhpgeez/mhpg:latest help`
4. CLI example Run: `docker run -it --rm -p "5001:5001" mhpgeez/mhpg:latest generate-list -t 2 -d 1,2,3 -v `

## Sources <a id='ssSources'></a>
- https://www.youtube.com/watch?v=U7TY_qUD8yA
- https://github.com/marketplace/actions/build-and-push-docker-images
- https://discourse.nixos.org/t/how-to-use-nix-only-in-docker-for-a-project/18043
- https://stackoverflow.com/questions/48470049/build-a-json-string-with-bash-variables
- https://stackoverflow.com/questions/3795470/how-do-i-get-just-real-time-value-from-time-command
- https://community.unix.com/t/storing-output-of-time-command-to-a-variable/281158/2
- https://dev.to/aws-builders/running-jobs-in-a-container-via-github-actions-securely-p0c
- https://github.com/rishabkumar7/aws-devops-capstone-project/blob/main/.github/workflows/build-docker.yaml
- https://goobar.dev/manually-triggering-github-actions-workflows/
- https://forum.golangbridge.org/t/killing-child-process-on-timeout-in-go-code/995
- https://stackoverflow.com/a/67752977
- https://jarv.org/posts/command-with-timeout/
- https://qmacro.org/blog/posts/2021/03/26/mass-deletion-of-github-actions-workflow-runs/
- https://github.com/orgs/community/discussions/25725

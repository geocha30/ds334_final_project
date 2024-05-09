## We Need More Data!

## Introduction 

As an intern at the Sub-Saharan Africa Poverty Team with the World Bank Group, I recognized the critical issue of insufficient data regarding underdeveloped countries, hindering in-depth research and comprehensive solutions. This project aims to construct an effective classification model for underdeveloped countries using variables from The World Bank Data Repository. Challenges in variable selection arose due to data sparsity, leading to a focus on post-2000 data. Logistic regression and neural networks were explored as primary analytical methods.

## Variable Description

The dataset incorporates means and standard deviations of the following variables:

| Variable             | Description                                                                                                                                                                                           |
|------------------|------------------------------------------------------|
| agriculture_land     | The share of land area that is arable, under permanent crops, and under permanent pastures                                                                                                            |
| birth_life_exp       | The number of years a newborn infant would live if prevailing patterns of mortality at the time of its birth were to stay the same throughout its life                                                |
| electricity_access   | The percentage of population with access to electricity                                                                                                                                               |
| fertility_rate       | The number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates of the specified year |
| internet             | The percentage of population that uses the internet                                                                                                                                                   |
| population_growth    | Annual population growth rate for year t is the exponential rate of growth of midyear population from year t-1 to t, expressed as a percentage                                                        |
| primary_school_enrol | Gross enrollment ratio is the ratio of total enrollment, regardless of age, to the population of the age group that officially corresponds to the level of education shown                            |
| sanitation           | Percentage of people using at least basic sanitation services, that is, improved sanitation facilities that are not shared with other households                                                      |
| total_unemployment   | The share of the labor force that is without work but available for and seeking employment                                                                                                            |

## Logistic Regression Model Attempts

Initial attempts using logistic regression models highlighted limitations in capturing the dataset's complexities, prompting further exploration of more sophisticated modeling approaches. Despite efforts to refine the models, logistic regression proved inadequate in interpreting the complex relationships among variables.

## Neural Network Application

In response to logistic regression's shortcomings, a neural network, a more complex machine learning model, was employed. Trained on a Colab Notebook, the neural network exhibited significant promise, achieving a remarkable 95.65% accuracy. Visual representations, including maps of actual and predicted classifications, underscored the model's effectiveness.

## Conclusion

While the neural network displayed promising results, discrepancies between actual and predicted classifications revealed challenges, particularly in accurately identifying underdeveloped countries. The project emphasizes the urgent need for more comprehensive data on underdeveloped regions to facilitate accurate modeling and targeted research efforts. It invites further exploration and collaboration to expand datasets and derive more precise insights, essential for addressing global development challenges.

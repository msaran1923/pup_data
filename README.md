<h1 class="code-line" data-line-start=0 data-line-end=1 ><a id="Project_Files_Description_0"></a>Project Files Description</h1>
<h2 class="code-line" data-line-start=2 data-line-end=3 ><a id="Analysis_Scripts_2"></a>Analysis Scripts</h2>
<ul>
<li class="has-line-data" data-line-start="3" data-line-end="5"><code>all_analysis.R</code> - Comprehensive R script containing all statistical analyses performed in the study, including data preprocessing, statistical tests, and visualization code. Contains embedded quantitative survey response data as data frames. This is the main analysis file that generates all results and plots.</li>
</ul>
<h2 class="code-line" data-line-start=5 data-line-end=6 ><a id="Data_Files_5"></a>Data Files</h2>
<ul>
<li class="has-line-data" data-line-start="6" data-line-end="8"><code>open_ended_responses.csv</code> - Raw data file containing qualitative responses from study participants. Contains unstructured text data for qualitative analysis.</li>
</ul>
<h2 class="code-line" data-line-start=8 data-line-end=9 ><a id="Results_and_Outputs_8"></a>Results and Outputs</h2>
<ul>
<li class="has-line-data" data-line-start="9" data-line-end="11"><code>analysis_results.txt</code> - Text file containing detailed statistical output, including test statistics, p-values, effect sizes, and model summaries from the analyses performed in <code>all_analysis.R</code>.</li>
</ul>
<h2 class="code-line" data-line-start=11 data-line-end=12 ><a id="Visualizations_11"></a>Visualizations</h2>
<ul>
<li class="has-line-data" data-line-start="12" data-line-end="13"><code>boxplots.png</code> - Box and whisker plots showing the distribution of key variables, including outliers and quartile information.</li>
<li class="has-line-data" data-line-start="13" data-line-end="14"><code>correlation_heatmap.png</code> - Heatmap visualization showing the strength and direction of correlations between study variables.</li>
<li class="has-line-data" data-line-start="14" data-line-end="15"><code>histograms.png</code> - Frequency distribution plots showing the shape and spread of continuous variables in the dataset.</li>
<li class="has-line-data" data-line-start="15" data-line-end="17"><code>response_distributions.png</code> - Visualization of response patterns across different categories or groups in the study.</li>
</ul>
<h2 class="code-line" data-line-start=17 data-line-end=18 ><a id="File_Organization_17"></a>File Organization</h2>
<pre><code class="has-line-data" data-line-start="19" data-line-end="29">.
├── all_analysis.R              # Main analysis script (includes quantitative survey data)
├── analysis_results.txt        # Statistical output
├── open_ended_responses.csv    # Raw data
└── visualizations/
    ├── boxplots.png
    ├── correlation_heatmap.png
    ├── histograms.png
    └── response_distributions.png
</code></pre>

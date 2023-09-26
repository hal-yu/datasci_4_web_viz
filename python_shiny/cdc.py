from shiny import App, render, ui
import matplotlib.pyplot as plt
import pandas as pd

# Load the dataset
def load_data():
    url = "https://raw.githubusercontent.com/hal-yu/datasci_4_web_viz/main/data/California.csv"
    return pd.read_csv(url)

df = load_data()
df_diabetes = df[(df['MeasureId'] == 'DIABETES') & (df['Data_Value_Type'] == 'Age-adjusted prevalence')]

# Available counties for selection
counties = df_diabetes['LocationName'].unique()

app_ui = ui.page_fluid(
    ui.input_select("county", "Select County", {county: county for county in counties}),
    ui.output_text_verbatim("avg_data_value"),
    ui.output_plot("bar_chart")
)

def server(input, output, session):

    @output
    @render.text
    def avg_data_value():
        selected_county = input.county()
        avg_value = df_diabetes[df_diabetes['LocationName'] == selected_county]['Data_Value'].mean()
        return f"Average Diabetes Age-adjusted Prevalence for {selected_county}: {avg_value:.2f}%"

    @output
    @render.plot(alt="Diabetes Age-adjusted Prevalence Bar Chart")
    def bar_chart():
        overall_avg = df_diabetes['Data_Value'].mean()
        selected_county_avg = df_diabetes[df_diabetes['LocationName'] == input.county()]['Data_Value'].mean()

        fig, ax = plt.subplots(figsize=(10, 6))
        ax.bar(['Selected County', 'Overall Average'], [selected_county_avg, overall_avg], color=['lightcoral', 'dodgerblue'])
        
        ax.set_ylabel('Data Value (Age-adjusted prevalence) - Percent')
        ax.set_ylim(0, 30)
        ax.set_title('Diabetes Age-adjusted Prevalence Comparison')
        
        return fig


app = App(app_ui, server)
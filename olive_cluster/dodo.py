from doit import create_after

def task_cleanup():
    return {
        'file_dep': ['data/olive.csv', 'scripts/load_data.py'],
        'targets': ['data/olive_clean.csv'],
        'actions': ['python scripts/load_data.py'],
    }

def task_cluster():
    return {
        'file_dep': ['data/olive_clean.csv', 'scripts/cluster.py'],
        'targets': ['data/olive_clustered.csv'],
        'actions': ['python scripts/cluster.py'],
    }

def task_report():
    return {
        'file_dep': ['data/olive_clustered.csv', 'scripts/make_report.py'],
        'targets': ['report.png'],
        'actions': ['python scripts/make_report.py'],
    }

def task_tests():
    return {
        'actions': ['pytest'],
        'file_dep': ['scripts/load_data.py', 'tests/test_load_data.py'],
    }

def task_render_quarto():
    """Render the Quarto report."""
    return {
        'actions': ['quarto render report.qmd'],
        'file_dep': ['report.qmd', 'data/olive_clustered.csv'],
        'targets': ['report.html'],  # or whatever output file Quarto generates
        'clean': True,
        'verbosity': 2,
    }

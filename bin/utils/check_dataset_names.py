import re

def validate_block(name, pattern, block_name):
    """ Helper function to validate individual blocks and provide feedback """
    if block_name=="ME-PS":
        match = re.fullmatch(pattern, name)
        if not match:
            return False, "Invalid ME-PS format"

        me = match.group(1)
        ps = match.group(3)  # Only available in ME-PS combinations

        if ps and me == ps:
            return False, "ME and PS cannot be the same"

        return True, "Valid ME-PS block"
    
    else:
        if re.match(pattern, name):
            return True, f"{block_name} block is valid."
        else:
            return False, f"Invalid {block_name} block."

def validate_dataset_name(dataset_name):
    # Define regex patterns for different blocks
    process_pattern = r".*" # PROCESS is mandatory
    binning_pattern = r"Bin-[\w-]+"  # BINNING is optional
    filter_pattern = r"Fil-[\w-]+"  # FILTER is optional
    param_pattern = r"Par-[\w-]+"  # PARAMETERS is optional
    tune_pattern = r"Tune+"  # TUNE is mandatory 
    beame_pattern = r"13p6TeV|\d+TeV|\d+GeV"  # BEAME is mandatory
    me_ps_pattern = (
    r"(pythia6|pythia8|pythia8-evtgen|herwig6|herwigpp|herwig7|sherpa|sherpaMEPS|"
    r"(madgraph|madgraphMLM|amcatnloFXFX|madgraph-madspin|madgraphMLM-madspin|"
    r"amcatnloFXFX-madspin|amcatnlo|amcatnlo-madspin|alpgen|mcatnlo|powheg|"
    r"powheg-madspin|powheg-JHUGenV\d*|powhegMINLO|powhegMINNLO|powhegMINLO-JHUGenV\d*|"
    r"powhegMINNLO-JHUGen\d*|JHUGen|hardcol|bcvegpy2)"
    r"-(pythia6|pythia8|pythia8-evtgen|herwig6|herwigpp|herwig7))"
    )
    blocks = dataset_name.split('_')

    feedback = []
    valid = True

    # Step-by-step validation of blocks:

    # Validate PROCESS (first block is mandatory)
    if len(blocks) >= 1:
        process_match, msg = validate_block(blocks[0], process_pattern, "PROCESS")
        feedback.append(msg)
        valid &= process_match
    else:
        feedback.append("Missing PROCESS block.")
        valid = False

    current_index = 1

    # Validate BINNING (optional, second block if it starts with 'Bin-')
    if len(blocks) > current_index and blocks[current_index].startswith('Bin-'):
        binning_match, msg = validate_block(blocks[current_index], binning_pattern, "BINNING")
        feedback.append(msg)
        valid &= binning_match
        current_index += 1
    else:
        feedback.append("BINNING block is missing or optional.")

    # Validate FILTER (optional, next block if it starts with 'Fil-')
    if len(blocks) > current_index and blocks[current_index].startswith('Fil-'):
        filter_match, msg = validate_block(blocks[current_index], filter_pattern, "FILTER")
        feedback.append(msg)
        valid &= filter_match
        current_index += 1
    else:
        feedback.append("FILTER block is missing or optional.")

    # Validate PARAMETERS (optional, next block if it starts with 'Par-')
    if len(blocks) > current_index and blocks[current_index].startswith('Par-'):
        param_match, msg = validate_block(blocks[current_index], param_pattern, "PARAMETERS")
        feedback.append(msg)
        valid &= param_match
        current_index += 1
    else:
        feedback.append("PARAMETERS block is missing or optional.")

    # Validate TUNE (mandatory)
    if len(blocks) > current_index:
        tune_match, msg = validate_block(blocks[current_index], tune_pattern, "TUNE")
        feedback.append(msg)
        valid &= tune_match
        current_index += 1
    else:
        feedback.append("Missing TUNE block.")
        valid = False

    # Validate BEAME (mandatory)
    if len(blocks) > current_index:
        beame_match, msg = validate_block(blocks[current_index], beame_pattern, "BEAME")
        feedback.append(msg)
        valid &= beame_match
        current_index += 1
    else:
        feedback.append("Missing BEAME block.")
        valid = False

    # Validate ME-PS (mandatory)
    if len(blocks) > current_index:
        me_ps_match, msg = validate_block(blocks[current_index], me_ps_pattern, "ME-PS")
        feedback.append(msg)
        valid &= me_ps_match
    else:
        feedback.append("Missing ME-PS block.")
        valid = False

    # Return feedback and validity status
    if valid:
        return True, "Valid dataset name", feedback
    else:
        return False, ("Invalid dataset name.\nThe strucuture of the name should be:\n"+
	               "PROCESS_[BINNING]_[FILTER]_[PARAMETERS]_TUNE_BEAME_ME-PS\n"+
		       "For more details please check:\n"+
		       "https://cms-pdmv.gitbook.io/project/mccontact/rules-for-run3-2024-dataset-names\n\n"+
		       "Below output for experts only"), feedback


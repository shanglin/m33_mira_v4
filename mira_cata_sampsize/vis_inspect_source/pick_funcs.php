<?php

function validate_al_num_und($str) {
	return preg_match('`^[a-zA-Z0-9_]{1,}$`',$str);
}

function validate_num_und($str) {
	return preg_match('`^[0-9_]{1,}$`',$str);
}

class disfig {
  public function setID ($figid) {
    $this->figID = $figid;
    $this->src = $_SESSION['figs'][$this->figID];
    $this->uid = str_replace($_SESSION['imgdir'],'',$this->src); 
    $this->frm = '<label><input type="radio" name="'.$this->uid.'" value=9>Yes </label>' .
      '<label><input type="radio" name="'.$this->uid.'" value=1 checked>No </label>' . 
      '<label><input type="radio" name="'.$this->uid.'" value=2>Not Sure </label>' .
      '<label><input type="radio" name="'.$this->uid.'" value=3>UD1 </label>' .
      '<label><input type="radio" name="'.$this->uid.'" value=4>UD2 </label>';
  }
}
?>
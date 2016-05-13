<?php
if (session_status() === PHP_SESSION_NONE){session_start();}
require_once("../source/pick_funcs.php");
include "../source/header.html";
$_SESSION["initial"] = "F";
$_SESSION['imgdir'] = 'pngs/';

$_SESSION['case'] = '0';
if (
    $_SERVER['REQUEST_METHOD'] === 'POST' and
    isset($_POST['post_type']) and
    $_POST['post_type'] == 'I' and
    isset($_POST['start_from']) and
    isset($_POST['n_figures_per_row']) and
    isset($_POST['n_rows_per_page']) and 
    validate_num_und($_POST['start_from']) and 
    validate_num_und($_POST['n_figures_per_row']) and
    validate_num_und($_POST['n_rows_per_page'])
    ) {
  $_SESSION['case'] = '1';
}

if (
    $_SERVER['REQUEST_METHOD'] === 'POST' and
    isset($_POST['post_type']) and
    $_POST['post_type'] == 'W'
    ) {
  $_SESSION['case'] = '2';
}

if (
    $_SERVER['REQUEST_METHOD'] === 'POST' and
    isset($_POST['post_type']) and
    $_POST['post_type'] == 'B'
    ) {
  $_SESSION['case'] = '3';
}

switch($_SESSION['case']) {
case '0':
  step_0();
  break;
case '1':
  step_1();
  break;
case '2':
  step_2();
  break;
case '3':
  step_3();
  break;
default:
  step_0();
}

function step_0() {
  include "../source/frm_initial.html";
  $_SESSION["figs"] = glob($_SESSION['imgdir']."*.png");
  shuffle($_SESSION["figs"]);
  $_SESSION["nfigs"] = count($_SESSION["figs"]);
  echo "<h3>There are ".$_SESSION["nfigs"]." figures in total. </h3><br>";
}
function step_1() {
  
  $_SESSION["start_from"] = $_POST['start_from'];
  $_SESSION["nfig_per_row"] = $_POST['n_figures_per_row'];
  $_SESSION["nrow_per_page"] = $_POST['n_rows_per_page'];
  $_SESSION['fig_start_id'] = $_SESSION["start_from"];
  step_2();
}
function step_3() {
  $_SESSION['fig_start_id'] = $_SESSION['fig_start_id'] - 2*$_SESSION["nfig_per_page"];  
  step_2();
}

function step_2() {
  //  include '../source/frm_goback.html';
  
  $_SESSION["nfig_per_page"] = $_SESSION["nfig_per_row"] * $_SESSION["nrow_per_page"];
  if (!isset($_SESSION['fig_start_id']) or $_SESSION['fig_start_id'] < 0) {
    $_SESSION['fig_start_id'] = 0;
  }
  $_SESSION['fig_end_id'] = $_SESSION['fig_start_id'] + $_SESSION["nfig_per_page"];
  $_SESSION['fig_end_id'] = min($_SESSION['fig_end_id'],$_SESSION["nfigs"]);
  $_SESSION['div_width'] = round(floor(99./$_SESSION["nfig_per_row"]));

  if (
      $_SERVER['REQUEST_METHOD'] === 'POST' and
      isset($_POST['post_type']) and
      $_POST['post_type'] == 'W'
      ) {
    // make the file names to be unique.
    $icount = 0;
    foreach ($_POST as $key => $value){
      $f_csv = $key.'.csv';
      if ($icount == 1) {
	break;
      }
      $icount++;
    }
    // print results to some .CSV files
    $csvdir = 'outcsvs/';
    if (!is_dir($csvdir)) {
      mkdir($csvdir,0755);
    }

    $file_exist = glob($csvdir . "*.csv");
    if (count($file_exist) > 0) {
      $histcsvdir = 'outcsvs/hist/';
      if (!is_dir($histcsvdir)) {
	mkdir($histcsvdir,0755);
      }
      foreach ($file_exist as $file_2move) {
	rename($file_2move,$histcsvdir . str_replace('outcsvs/','',$file_2move));
      }
    }
    
    
    $h_csv = fopen($csvdir.$f_csv,'w');
    foreach ($_POST as $key => $value){
      if ($key != 'post_type') {
	fwrite($h_csv, "{$key},{$value}\n");
      }
    }
    fclose($h_csv);
  }
  
  if ($_SESSION['fig_start_id'] < $_SESSION['fig_end_id']) {
    echo '<Form action="" method="post">';
    echo '<Input Type="hidden" Name="post_type" value="W">';
    echo 'Images from '.$_SESSION['fig_start_id'].' to '.($_SESSION['fig_end_id']-1).'<br>';
    for ($iimg=$_SESSION['fig_start_id']; $iimg<$_SESSION['fig_end_id']; $iimg++) {
      $figure = new disfig;
      $figure->setID($iimg);
      echo '<div style="width:'.$_SESSION['div_width'].'%;float:left;border:1px solid lightgreen;padding-bottom:30px;">
                <div style="width:100%;">
                  <img src="'.$figure->src.'" style="width:100%;">
                </div>
                <div style="width:100%;text-align:center;">'.
                   $figure->frm
                .'</div>
           </div>';
    }
    echo '<div style="clear:left;height:300px;width:100%;text-align:center;">';
    echo '<div style="height:100px;"></div><input type="submit" value="Click to submit" style="width:300px;height:100px;background-color:lightgreen;font-size:100%;">';
    echo '</div></form>';
  } else {
    echo '<h1>Finished!</h1>';


    $protocol = strpos(strtolower($_SERVER['SERVER_PROTOCOL']),'https')  === FALSE ? 'http' : 'https';
    $host     = $_SERVER['HTTP_HOST'];
    $script   = $_SERVER['SCRIPT_NAME'];
    $currentUrl = $script;
    $foo = str_replace('/pick_ceph/f1vi/','',$script);
    $bar = str_replace('/index','',$foo);
    $newdir = $bar + 1;
    $newurl = $protocol . '://' . $host . '/pick_ceph/f1vi/' . $newdir . '/';
    echo '<center>';
    echo '<h2> Go to <a href="' . $newurl . '">' . $newurl . '</a></h2><div style="height:200px;"></div>';
    echo '<h2> Go to <a href="' . $newurl . '">' . $newurl . '</a></h2><div style="height:200px;"></div>';
    echo '<h2> Go to <a href="' . $newurl . '">' . $newurl . '</a></h2><div style="height:200px;"></div>';
    echo '<h2> Go to <a href="' . $newurl . '">' . $newurl . '</a></h2><div style="height:200px;"></div>';
    echo '<h2> Go to <a href="' . $newurl . '">' . $newurl . '</a></h2><div style="height:200px;"></div>';
    echo '</center>';
  }
  $_SESSION['fig_start_id'] = $_SESSION['fig_start_id'] + $_SESSION["nfig_per_page"];    
}
include "../source/footer.html";
?>
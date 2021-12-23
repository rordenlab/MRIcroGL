{$ifdef windows}
  {$define PYTHON_DYNAMIC}
{$endif}
{$ifdef linux}
  //we now set this option in the .lpi file
  //{$define PYTHON_DYNAMIC}
{$endif}
{$ifdef darwin}
  //{$define PYTHON_DYNAMIC}
{$endif}

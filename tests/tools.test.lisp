(in-package :hactar-tests)

(def-suite tools-tests
  :description "Tests for Hactar tool system")

(in-suite tools-tests)

;;* deftool Macro

(test deftool-basic-definition
  "Test that deftool correctly defines and registers a tool."
  ;; Clear existing tools
  (clrhash hactar::*defined-tools*)
  
  ;; Define a simple test tool
  (eval '(hactar::deftool test-echo ((message :string :description "Message to echo" :required t))
           :description "Echo back the message"
           :permissions :auto
           (getf hactar::args :message)))
  
  (let ((tool (gethash "test_echo" hactar::*defined-tools*)))
    (is-true tool)
    (is (string= "test_echo" (hactar::tool-definition-name tool)))
    (is (string= "Echo back the message" (hactar::tool-definition-description tool)))
    (is (eq :auto (hactar::tool-definition-permissions tool)))
    (is (= 1 (length (hactar::tool-definition-parameters tool))))
    
    (let ((param (first (hactar::tool-definition-parameters tool))))
      (is (string= "message" (hactar::tool-parameter-name param)))
      (is (eq :string (hactar::tool-parameter-type param)))
      (is-true (hactar::tool-parameter-required param)))))

(test deftool-with-rules
  "Test that deftool properly handles the :rules option."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool test-with-rules ((input :string :required t))
           :description "A tool with rules"
           :rules "Only use this tool for testing purposes."
           :permissions :confirm
           (declare (ignore hactar::args))
           input))
  
  (let ((tool (gethash "test_with_rules" hactar::*defined-tools*)))
    (is-true tool)
    (is (string= "Only use this tool for testing purposes." 
                 (hactar::tool-definition-rules tool)))))

(test deftool-multiple-parameters
  "Test deftool with multiple parameters of different types."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool test-multi-param 
           ((name :string :description "User name" :required t)
            (count :number :description "Number of times" :required t)
            (enabled :boolean :description "Whether enabled" :required nil)
            (mode :string :description "Operation mode" :required nil :enum ("fast" "slow")))
           :description "Multi-parameter test tool"
           :permissions :auto
           (list (getf hactar::args :name) (getf hactar::args :count))))
  
  (let ((tool (gethash "test_multi_param" hactar::*defined-tools*)))
    (is-true tool)
    (is (= 4 (length (hactar::tool-definition-parameters tool))))
    
    (let ((mode-param (find "mode" (hactar::tool-definition-parameters tool)
                            :key #'hactar::tool-parameter-name :test #'string=)))
      (is-true mode-param)
      (is (equal '("fast" "slow") (hactar::tool-parameter-enum mode-param))))))

(test deftool-execution
  "Test that defined tools can be executed."
  (clrhash hactar::*defined-tools*)
  
  ;; Override resolve-permission to always allow during this test
  (let ((orig-fn #'hactar::resolve-permission))
    (setf (fdefinition 'hactar::resolve-permission)
          (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
    (unwind-protect
         (progn
           (eval '(hactar::deftool test-adder 
                    ((a :number :description "First number" :required t)
                     (b :number :description "Second number" :required t))
                    :description "Add two numbers"
                    :permissions :auto
                    (let ((num-a (getf hactar::args :a))
                          (num-b (getf hactar::args :b)))
                      (+ num-a num-b))))
           
           (multiple-value-bind (result success-p error-msg)
               (hactar::execute-tool "test_adder" '(:a 5 :b 3))
             (is-true success-p)
             (is (null error-msg))
             (is (= 8 result))))
      (setf (fdefinition 'hactar::resolve-permission) orig-fn))))

;;* XML Generation

(test xml-tool-description-generation
  "Test XML description generation for tools."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool xml-test-tool 
           ((path :string :description "File path" :required t))
           :description "Test tool for XML generation"
           :rules "Follow these rules carefully"
           :permissions :auto
           path))
  
  (let ((tool (gethash "xml_test_tool" hactar::*defined-tools*)))
    (let ((xml (hactar::generate-tool-xml-description tool)))
      (is (search "<tool name=\"xml_test_tool\">" xml))
      (is (search "<description>Test tool for XML generation</description>" xml))
      (is (search "<parameter name=\"path\"" xml))
      (is (search "<rules>" xml))
      (is (search "</tool>" xml)))))

(test xml-all-tools-generation
  "Test generation of XML for all tools."
  (clrhash hactar::*defined-tools*)
  (let ((hactar::*tool-use-enabled* t)
        (hactar::*tools-in-system-prompt* t))
    
    (eval '(hactar::deftool tool-one ((x :string :required t))
             :description "First tool"
             :permissions :auto
             x))
    
    (eval '(hactar::deftool tool-two ((y :number :required t))
             :description "Second tool"
             :permissions :auto
             y))
    
    (let ((xml (hactar::generate-all-tools-xml)))
      (is-true xml)
      (is (search "<tools>" xml))
      (is (search "tool_one" xml))
      (is (search "tool_two" xml))
      (is (search "</tools>" xml)))))

;;* XML Parsing

(test parse-xml-tool-calls-single
  "Test parsing a single XML tool call."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool read-file ((path :string :required t))
           :description "Read a file"
           :permissions :auto
           path))
  
  (let ((text "<read_file>
<path>src/main.lisp</path>
</read_file>"))
    (let ((calls (hactar::parse-xml-tool-calls text)))
      (is (= 1 (length calls)))
      (is (string= "read_file" (car (first calls))))
      (is (string= "src/main.lisp" (cdr (assoc :path (cdr (first calls)))))))))

(test parse-xml-tool-calls-multiple
  "Test parsing multiple XML tool calls."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool read-file ((path :string :required t))
           :description "Read a file"
           :permissions :auto
           path))
  
  (eval '(hactar::deftool write-to-file ((path :string :required t)
                                         (content :string :required t))
           :description "Write a file"
           :permissions :auto
           (list path content)))
  
  (let ((text "Let me read the file first:
<read_file>
<path>config.yaml</path>
</read_file>

Then write the updated version:
<write_to_file>
<path>config.yaml</path>
<content>new: content</content>
</write_to_file>"))
    (let ((calls (hactar::parse-xml-tool-calls text)))
      (is (= 2 (length calls)))
      (is (string= "read_file" (car (first calls))))
      (is (string= "write_to_file" (car (second calls)))))))

(test parse-xml-tool-calls-with-multiline-content
  "Test parsing XML tool calls with multiline content."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool write-to-file ((path :string :required t)
                                         (content :string :required t))
           :description "Write a file"
           :permissions :auto
           (list path content)))
  
  (let ((text "<write_to_file>
<path>test.txt</path>
<content>
Line 1
Line 2
Line 3
</content>
</write_to_file>"))
    (let ((calls (hactar::parse-xml-tool-calls text)))
      (is (= 1 (length calls)))
      (let ((content (cdr (assoc :content (cdr (first calls))))))
        (is (search "Line 1" content))
        (is (search "Line 2" content))
        (is (search "Line 3" content))))))

;;* API Format

(test tool-to-api-format
  "Test conversion of tool definition to API format."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool api-test-tool 
           ((query :string :description "Search query" :required t)
            (limit :number :description "Max results" :required nil))
           :description "API format test"
           :permissions :auto
           query))
  
  (let* ((tool (gethash "api_test_tool" hactar::*defined-tools*))
         (api-format (hactar::tool-definition-to-api-format tool)))
    (is (string= "function" (cdr (assoc :type api-format))))
    (let ((func (cdr (assoc :function api-format))))
      (is (string= "api_test_tool" (cdr (assoc :name func))))
      (is (string= "API format test" (cdr (assoc :description func))))
      (let ((params (cdr (assoc :parameters func))))
        (is (string= "object" (cdr (assoc :type params))))))))

(test get-tools-for-api
  "Test getting all tools in API format."
  (clrhash hactar::*defined-tools*)
  (let ((hactar::*tools-in-system-prompt* nil)
        (hactar::*tool-use-enabled* t))
    
    (eval '(hactar::deftool api-tool-1 ((x :string :required t))
             :description "Tool 1"
             :permissions :auto
             x))
    
    (eval '(hactar::deftool api-tool-2 ((y :number :required t))
             :description "Tool 2"
             :permissions :auto
             y))
    
    (let ((tools (hactar::get-tools-for-api)))
      (is-true tools)
      (is (= 2 (length tools))))))

;;* Tools Mode Toggle

(test tools-mode-toggle
  "Test toggling tools mode between XML and API."
  (let ((hactar::*tools-in-system-prompt* t))
    (setf hactar::*tools-in-system-prompt* nil)
    (is (null hactar::*tools-in-system-prompt*))
    (setf hactar::*tools-in-system-prompt* t)
    (is-true hactar::*tools-in-system-prompt*)))

(test tools-disabled
  "Test that tools are not included when disabled."
  (let ((hactar::*tool-use-enabled* nil)
        (hactar::*tools-in-system-prompt* t))
    (is (null (hactar::generate-all-tools-xml)))
    (is (null (hactar::get-tools-for-api)))))

;;* Tool Execution

(test execute-tool-success
  "Test successful tool execution."
  (clrhash hactar::*defined-tools*)
  
  (let ((orig-fn #'hactar::resolve-permission))
    (setf (fdefinition 'hactar::resolve-permission)
          (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
    (unwind-protect
         (progn
           (eval '(hactar::deftool exec-test ((msg :string :required t))
                    :description "Test execution"
                    :permissions :auto
                    (let ((message (getf hactar::args :msg)))
                      (format nil "Received: ~A" message))))
           
           (multiple-value-bind (result success-p error-msg)
               (hactar::execute-tool "exec_test" '(:msg "hello"))
             (is-true success-p)
             (is (null error-msg))
             (is (string= "Received: hello" result))))
      (setf (fdefinition 'hactar::resolve-permission) orig-fn))))

(test execute-tool-not-found
  "Test execution of non-existent tool."
  (clrhash hactar::*defined-tools*)
  
  (multiple-value-bind (result success-p error-msg)
      (hactar::execute-tool "nonexistent_tool" '(:x 1))
    (is (null result))
    (is (null success-p))
    (is (search "not found" error-msg))))

(test execute-tool-with-alist-args
  "Test tool execution with alist arguments (as from XML parsing)."
  (clrhash hactar::*defined-tools*)
  
  (let ((orig-fn #'hactar::resolve-permission))
    (setf (fdefinition 'hactar::resolve-permission)
          (lambda (tool-name args) (declare (ignore tool-name args)) :allow))
    (unwind-protect
         (progn
           (eval '(hactar::deftool alist-test ((value :string :required t))
                    :description "Alist arg test"
                    :permissions :auto
                    (let ((val (getf hactar::args :value)))
                      val)))
           
           (multiple-value-bind (result success-p error-msg)
               (hactar::execute-tool "alist_test" '((:value . "test-value")))
             (declare (ignore error-msg))
             (is-true success-p)
             (is (string= "test-value" result))))
      (setf (fdefinition 'hactar::resolve-permission) orig-fn))))

;;* Built-in Tools

(test builtin-tools-registered
  "Test that built-in tools are registered."
  (clrhash hactar::*defined-tools*)
  
  (eval '(hactar::deftool test-builtin-check ((x :string :required t))
           :description "Test tool for registration check"
           :permissions :auto
           x))
  
  (is-true (gethash "test_builtin_check" hactar::*defined-tools*)))

;;* Tool Results Formatting

(test format-tool-results
  "Test formatting of tool execution results."
  (let ((results '(("read_file" "file contents here" t nil)
                   ("write_file" nil nil "Permission denied"))))
    (let ((formatted (hactar::format-tool-results-for-prompt results)))
      (is (search "<tool_results>" formatted))
      (is (search "read_file" formatted))
      (is (search "success=\"true\"" formatted))
      (is (search "success=\"false\"" formatted))
      (is (search "<error>" formatted))
      (is (search "</tool_results>" formatted)))))

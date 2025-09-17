/**
 * Stepper functionality for California Soil Health Reports
 * Handles step navigation and mobile progress bar updates
 */

function setStep(element, inputId) {
  const stepNumber = element.getAttribute("data-step");
  
  // Don't allow navigation to disabled steps
  if (element.classList.contains("disabled")) {
    return;
  }

  // Update UI - remove active class from all steps
  document.querySelectorAll(".step").forEach(el => el.classList.remove("active"));
  
  // Add active class to clicked step
  element.classList.add("active");

  // Update mobile progress bar
  updateMobileProgressBar(parseInt(stepNumber));

  // Let Shiny know (use correct namespaced inputId!)
  Shiny.setInputValue(inputId, parseInt(stepNumber), { priority: "event" });
}

function updateMobileProgressBar(currentStep) {
  const totalSteps = 8; // California Soil Health Reports has 8 steps
  const progressPercent = (currentStep / totalSteps) * 100;
  
  const stepTextElement = document.getElementById("progress-step-text");
  if (stepTextElement) {
    stepTextElement.textContent = `${currentStep}/${totalSteps}`;
  }
  
  const progressBarElement = document.getElementById("progress-bar");
  if (progressBarElement) {
    progressBarElement.style.width = `${progressPercent}%`;
  }
}

// Initialize stepper on page load
document.addEventListener('DOMContentLoaded', function() {
  // Set initial progress bar state
  updateMobileProgressBar(1);
  
  // Add click handlers for step navigation
  document.querySelectorAll('.step').forEach(step => {
    step.addEventListener('click', function() {
      const inputId = this.getAttribute('onclick');
      if (inputId) {
        // Extract the input ID from the onclick attribute
        const match = inputId.match(/setStep\(this, '([^']+)'\)/);
        if (match) {
          setStep(this, match[1]);
        }
      }
    });
  });
});

// Utility function to update step states
function updateStepState(stepNumber, state) {
  const stepElement = document.querySelector(`[data-step="${stepNumber}"]`);
  if (stepElement) {
    // Remove existing state classes
    stepElement.classList.remove('active', 'completed', 'disabled', 'valid', 'invalid', 'loading');
    
    // Add new state class
    if (state) {
      stepElement.classList.add(state);
    }
  }
}

// Function to mark steps as completed
function markStepCompleted(stepNumber) {
  updateStepState(stepNumber, 'completed');
}

// Function to mark steps as valid/invalid
function markStepValid(stepNumber, isValid) {
  updateStepState(stepNumber, isValid ? 'valid' : 'invalid');
}

// Function to show loading state
function showStepLoading(stepNumber) {
  updateStepState(stepNumber, 'loading');
}

// Function to hide loading state
function hideStepLoading(stepNumber) {
  const stepElement = document.querySelector(`[data-step="${stepNumber}"]`);
  if (stepElement) {
    stepElement.classList.remove('loading');
  }
}

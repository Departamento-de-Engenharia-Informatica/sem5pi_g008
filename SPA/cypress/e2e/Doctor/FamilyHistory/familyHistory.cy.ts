import json from '../../../globalVariables.json';

describe('Add Family History Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
    cy.visit('http://localhost:4200/staff/patients/');

    const newName = 'new User';

    cy.contains('.patient-profile-card', newName)
      .find('.view-button')
      .click();
  });

  it('Visits the Medical Condition Management Component', () => {
    cy.contains('h1', 'Medical Record Details');
    cy.get('app-family-history').should('exist');
  });

  it('Should display "Add Family History" button initially', () => {
    // Verifica se o botão "Add Family History" está visível
    cy.get('app-family-history').within(() => {
      cy.get('button').contains('Add Family History').should('be.visible');
    });
  });

  it('Should show the form when "Add Family History" button is clicked', () => {
    // Clica no botão "Add Family History"
    cy.get('app-family-history').within(() => {
      cy.get('button').contains('Add Family History').click();

      // Verifica se o formulário foi exibido
      cy.get('form').should('be.visible');
    });
  });


  it('Should submit the form successfully when fields are valid', () => {
    cy.get('app-family-history').within(() => {
      cy.get('button').contains('Add Family History').click();

      cy.get('#familyMember').type('Father');
      cy.get('#condition').type('Diabetes');

      cy.get('button[type="submit"]').click();

      // Verifica se a mensagem de sucesso foi exibida
      cy.on('window:alert', (text) => {
        expect(text).to.contains('Family history saved successfully!');
      });
    });
  });


  it('Should enable the submit button when form fields are valid', () => {
    // Exibe o formulário e preenche os campos
    cy.get('app-family-history').within(() => {
      cy.get('button').contains('Add Family History').click();

      cy.get('#familyMember').type('Father');
      cy.get('#condition').type('Diabetes');

      // Verifica se o botão de submit está habilitado
      cy.get('button[type="submit"]').should('not.be.disabled');
    });
  });

  it('Should display "There is no family history" message when the list is empty', () => {

    cy.intercept('GET', '/api/family-history', {
      statusCode: 200,
      body: []
    });

    cy.contains('There is no family history for this patient').should('be.visible');
  });

});

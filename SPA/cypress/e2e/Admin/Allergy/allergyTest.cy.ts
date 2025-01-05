import json from '../../../globalVariables.json';

describe('Allergy Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Admin);
    cy.visit('/admin/allergyManagement');
  });

  it('Visits the Allergy Management Component', () => {
    cy.contains('h1', 'Allergy Management');
  });

  it('Update Designation of an Allergy', () => {
    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialDesignation = allergy.find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Designation:'))
        .text()
        .replace('Designation:', '')
        .trim();
      const updatedDesignation = `Updated ${initialDesignation}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#designation').clear().type(updatedDesignation);

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list .allergy-item').first().find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Designation:'))
        .should('have.text', `Designation: Updated ${initialDesignation}`);
    });
  });

  it('Update Description of an Allergy', () => {
    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialDescription = allergy.find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Description:'))
        .text()
        .replace('Description:', '')
        .trim();
      const updatedDescription = `Updated ${initialDescription}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#description').clear().type(updatedDescription);

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list .allergy-item').first().find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Description:'))
        .should('have.text', `Description: Updated ${initialDescription}`);
    });
  });

  it('Add and Remove an Effect from an Allergy', () => {
    const minuteAndSecond = new Date().toISOString().slice(14, 19).replace(':', '');

    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialEffects = allergy.find('.allergy-details ul li').map((index, el) => el.innerText).get();

      const newEffect = `Effect ${minuteAndSecond}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#effect').clear().type(newEffect);
      cy.get('.effect-input button').click();
      cy.get('.effects-list').should('contain', newEffect);
      cy.get('.effects-list .effect-item').first().find('button').click();
      cy.get('.effects-list').should('not.equal', initialEffects[0]);
      cy.get('[type="submit"]').click();
      cy.get('.allergy-list .allergy-item').first().find('.allergy-details ul li')
        .should('contain', newEffect)
        .and('not.contain', initialEffects[0]);
    });
  });

  describe('Add Allergy', () => {
    it('Add a new Allergy With All', () => {

      let initialCount = 0;

      cy.get('.allergy-list').its('length').then((length) => {
        initialCount = length;
      });
      const code = 'LK12';
      const designation = 'Lactose Intolerance';
      const description = 'Lactose intolerance is a digestive disorder caused by the inability to digest lactose, the main carbohydrate in dairy products. It can cause various symptoms, including bloating, diarrhea and abdominal cramps. People with lactose intolerance don\'t make enough of the enzyme lactase, which is needed to digest lactose.';
      const effect = 'Bloating';
      const effect2 = 'Diarrhea';

      cy.get('.add-button').click();

      cy.get('#allergyCode').type(code);
      cy.get('#allergyDesignation').type(designation);
      cy.get('#allergyDescription').type(description);
      cy.get('#allergyEffect').type(effect);
      cy.get('.effect-input > button').click();
      cy.get('#allergyEffect').type(effect2);
      cy.get('.effect-input > button').click();

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list').should('have.length', initialCount + 1);
    });

    it('Add a new Allergy With Code and Designation', () => {

      let initialCount = 0;

      cy.get('.allergy-list').its('length').then((length) => {
        initialCount = length;
      });
      const code = 'LK20';
      const designation = 'Lactose Allergy';

      cy.get('.add-button').click();

      cy.get('#allergyCode').type(code);
      cy.get('#allergyDesignation').type(designation);

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list').should('have.length', initialCount + 1);
    });
  });

  it('Add a new Medical Condition and handle invalid code error', () => {
    const minuteAndSecond = new Date().toISOString().slice(15, 19).replace(':', '');
    const code = "MC123123123";
    const designation = `Hypertension${minuteAndSecond}`;
    const description = 'High blood pressure';
    const symptom = 'Headache';

    cy.intercept('POST', 'http://localhost:4000/api/medicalConditions', {
      statusCode: 805,
      body: {
        message: 'Invalid ICD-11 code.'
      }
    }).as('addMedicalConditionRequest');

    cy.get('.add-button').click();
    cy.get('#code').type(code);
    cy.get('#designation').type(designation);
    cy.get('#description').type(description);
    cy.get('#symptom').type(symptom);
    cy.get('.symptom-input > button').click();
    cy.get('[type="submit"]').click();

    cy.wait('@addMedicalConditionRequest').then((interception) => {
      console.log(interception);
      expect(interception.response!.statusCode).to.eq(805);
      expect(interception.response!.body.message).to.contain('Invalid ICD-11 code.');
    });
  });


  it('Add a new Allergy and handle invalid code error', () => {
    let initialCount = 0;

    cy.get('.allergy-list').its('length').then((length) => {
      initialCount = length;
    });

    const code = "A123123123";
    const designation = `Hypertension`;

    cy.intercept('POST', 'http://localhost:4000/api/allergy', {
      statusCode: 500,
      body: {
        message: 'An error occurred on the server. Please try again later.'
      }
    }).as('addAllergyRequest');

    cy.get('.add-button').click();
    cy.get('#allergyCode').type(code);
    cy.get('#allergyDesignation').type(designation);

    cy.get('[type="submit"]').click();

    cy.wait('@addAllergyRequest').then((interception) => {
      console.log(interception);
      expect(interception.response!.statusCode).to.eq(500);
      expect(interception.response!.body.message).to.eq('An error occurred on the server. Please try again later.');
    });
  });

});

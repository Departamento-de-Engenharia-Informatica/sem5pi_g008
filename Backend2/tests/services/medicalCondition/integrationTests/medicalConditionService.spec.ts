import 'reflect-metadata';
import * as sinon from 'sinon';
import { Container } from 'typedi';
import MedicalConditionService from '../../../../src/services/medicalConditionService';
import IMedicalConditionRepo from '../../../../src/services/IRepos/IMedicalConditionRepo';
import { MedicalCondition } from '../../../../src/domain/MedicalCondition/MedicalCondition';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';

describe('MedicalConditionService', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();
        let medicalConditionSchemaInstance = require('../../../../src/persistence/schemas/medicalConditionSchema').default;
        Container.set('medicalConditionSchema', medicalConditionSchemaInstance);

        let medicalConditionRepoClass = require('../../../../src/repos/MedicalConditionRepo').default;
        let medicalConditionRepoInstance = Container.get(medicalConditionRepoClass);
        Container.set('MedicalConditionRepo', medicalConditionRepoInstance);

        let medicalConditionServiceClass = require('../../../../src/services/medicalConditionService').default;
        let medicalConditionServiceInstance = Container.get(medicalConditionServiceClass);
        Container.set('medicalConditionService', medicalConditionServiceInstance);
    });

    afterEach(function () {
        sandbox.restore();
    });

    it('should create a medical condition successfully using service mock', async function () {
        // Arrange
        const medicalConditionDTO: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        
        let repoInstance = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(repoInstance, 'save').returns();
        

        let medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

        // Act
        await medicalConditionService.createMedicalCondition(medicalConditionDTO);

        // Assert
        sinon.assert.calledOnce(repoInstance.save);
        sinon.assert.calledWith(repoInstance.save, sinon.match.instanceOf(MedicalCondition), sinon.match.any);
    });
    
});

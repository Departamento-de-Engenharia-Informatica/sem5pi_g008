import {Inject, Service} from "typedi";
import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import config from "../../config";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import IMedicalConditionService from "../services/IServices/IMedicalConditionService";
import {NotFoundException} from "../Exceptions/NotFoundException";
import {AppError} from "../domain/Shared/Exceptions/AppError";

@Service()
export default class MedicalConditionController implements IMedicalConditionController {
  constructor(
    @Inject("MedicalConditionService") private medicalConditionServiceInstance: IMedicalConditionService
  ) {
  }
  public async searchMedicalConditionsCode(req: any, res: any) {
    const  query  = req.params.code; // Expecting a query string in the body
    console.log("Query: ", query);

    try {
      const medicalConditions = await this.medicalConditionServiceInstance.searchMedicalConditionsCode(query);
      console.log("Medical Conditions CONTROLLER: "+medicalConditions);

      res.status(200).json({ 
        data: medicalConditions,
      });
    } catch (error) {
      if (error instanceof AppError) {
        console.error(`AppError: ${error.code} - ${error.message}`);
        res.status(error.code).json({
          message: error.message,
        });
      } else {
        console.error('Error searching medical conditions:', error);
        res.status(500).json({
          message: 'An error occurred while searching for medical conditions.',
        });
      }
    }
  }
  public async searchMedicalConditionsDesignation(req: any, res: any) {
    const  query  = req.params.designation; // Expecting a query string in the body
    console.log("Query: ", query);

    try {
      const medicalConditions = await this.medicalConditionServiceInstance.searchMedicalConditionsDesignation(query);
      console.log("Medical Conditions CONTROLLER: "+medicalConditions);

      res.status(200).json({
        data: medicalConditions,
      });
    } catch (error) {
      if (error instanceof AppError) {
        console.error(`AppError: ${error.code} - ${error.message}`);
        res.status(error.code).json({
          message: error.message,
        });
      } else {
        console.error('Error searching medical conditions:', error);
        res.status(500).json({
          message: 'An error occurred while searching for medical conditions.',
        });
      }
    }
  }

  public async createMedicalCondition(req: any, res: any) {

    let medicalConditionDTO: IMedicalConditionDTO = req.body;

    try {

      await this.medicalConditionServiceInstance.createMedicalCondition(medicalConditionDTO);

      res.status(200).json({
        message: 'Medical Condition created successfully'
      });

    } catch (error) {

      if (error instanceof AppError) {

        if (error.code >= 800 && error.code <= 804) {
          console.log("Error: " + error.code + " - " + error.message);
            res.status(error.code).json({
                message: error.message
            });
        } else {
          res.status(error.code).json({
            message: error.message
          });
        }

      } else {
        res.status(500).json({
          message: 'Code/Designation already exists',
        });
      }
    }
  }

  public async getAllMedicalConditions(req: any, res: any) {
    try {
      const medicalConditions = await this.medicalConditionServiceInstance.getAllMedicalConditions();
      res.status(200).json({
        medicalConditions: medicalConditions
      });
    } catch (error) {
      res.status(500).json({
        message: 'Error fetching medical conditions' + error,
      });
    }
  }

  public async updateMedicalConditionDescription(req: any, res: any) {
    let id = req.params.id;
    let description = req.body.description;

    try {
      await this.medicalConditionServiceInstance.updateMedicalConditionDescription(id, description);
      res.status(200).json({
        message: 'Medical Condition description updated successfully'
      });
    } catch (error) {

      console.log("Error: " + error.message);
      if (error instanceof NotFoundException) {
        res.status(404).json({
          message: error.message
        });
      } else {
        res.status(500).json({
          message: error.message,
        });
      }
    }
  }

  public async updateMedicalConditionSymptoms(req: any, res: any) {
    let id = req.params.id;
    let symptomsList = req.body.symptomsList;

    try {
      await this.medicalConditionServiceInstance.updateMedicalConditionSymptoms(id, symptomsList);
      res.status(200).json({
        message: 'Medical Condition symptoms updated successfully'
      });
    } catch (error) {
      console.log("Error: " + error.message);
      if (error instanceof NotFoundException) {
        res.status(404).json({
          message: error.message
        });
      } else {
        res.status(500).json({
          message: 'Error updating medical condition symptoms' + error,
        });
      }
    }
  }
}

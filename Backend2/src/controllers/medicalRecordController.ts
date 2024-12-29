import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import {AppError} from "../domain/MedicalCondition/Exceptions/AppError";
import {
  MedicalConditionNotFoundException
} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {
  MedicalRecordConditionNotFoundException
} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {error} from "winston";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

  constructor(
    @Inject(config.services.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService,
  ) {}

  public async createMedicalRecord(req: any, res: any): Promise<void> {

    let medicalRecordId = req.body.recordNumberId;

    try {
      await this.medicalRecordInstance.createMedicalRecord(medicalRecordId);
      res.status(201).json({
        message: 'Medical record created successfully',
      });
    } catch (error) {
      if (error.code === 11000) {
        console.error('Error creating medical record:', error.message);
        res.status(409).json({
          message: 'Error creating medical record - Duplicate entry',
        });
      } else {
        console.error('Error creating medical record:', error.message);
        res.status(500).json({
          message: 'Error creating medical record',
          details: error.message
        });
      }
    }
  }

  public async getMedicalRecordConditionByCode(req: any, res: any) {

    try {

      let medicalRecordId = req.params.id;
      let conditionCode = req.params.code;

      console.log(medicalRecordId, conditionCode);


      const medicalRecordConditionDTO = await this.medicalRecordInstance.getMedicalRecordConditionByCode(medicalRecordId, conditionCode);

      res.status(200).json({
        medicalRecordCondition: medicalRecordConditionDTO
      });

    } catch (error) {
      if (error instanceof NoMedicalRecordException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof AppError) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof MedicalConditionNotFoundException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof MedicalRecordConditionNotFoundException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      console.log(error);

      res.status(500).json({
        message: 'Error getting medical record condition.',

      });
    }
  }

  public async getMedicalRecordConditionByDesignation(req: any, res: any) {

    try {

      let medicalRecordId = req.params.id;
      let conditionDesignation = req.params.designation;

      console.log(medicalRecordId, conditionDesignation);

      const medicalRecordConditionDTO = await this.medicalRecordInstance.getMedicalRecordConditionByDesignation(medicalRecordId, conditionDesignation);

      res.status(200).json({
        medicalRecordCondition: medicalRecordConditionDTO
      });

    } catch (error) {
      if (error instanceof NoMedicalRecordException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof AppError) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof MedicalConditionNotFoundException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof MedicalRecordConditionNotFoundException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      console.log(error);

      res.status(500).json({
        message: 'Error getting medical record conditions.',

      });
    }
  }


  public async getMedicalRecordConditions(req: any, res: any): Promise<void> {
    try {

      let medicalRecordId = req.params.id;

      const medicalConditionDTOList = await this.medicalRecordInstance.getMedicalRecordConditions(medicalRecordId);

      res.status(200).json({
        medicalRecordConditions: medicalConditionDTOList
      });

    } catch (error) {

      if (error instanceof NoMedicalRecordConditionsException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof NoMedicalRecordException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      res.status(500).json({
        message: 'Error getting medical record conditions.',

      });
    }
  }

  public async getAllergies(req: any, res: any): Promise<IMedicalRecordAllergyDTO[]> {
    let medicalRecordId = req.params.id;

    try {
      res.status(200).json({
        body: await this.medicalRecordInstance.getAllergies(medicalRecordId)
      });
    } catch (error) {
      console.error('Error getting allergies:', error.message);
      res.status(500).json({
        message: 'Error getting allergies',
        details: error.message
      });
    }

    return null;
  }

  public async addFreeText(req: any, res: any): Promise<any> {
    let freeText: IMedicalRecordFreeTextDTO = req.body;

    try {
      await this.medicalRecordInstance.addFreeText(freeText);

      res.status(200).json({
        message: 'Comment Added Successfully'
      });

    } catch (error) {
      console.error('Error adding comment:', error.message);

    }
  }

  createFamilyHistory(req: any, res: any, next: any): void {
    try {
      console.log("Creating family history for medical record with ID:", req.body.medicalRecordID);
      console.log("Family history:", req.body.familyHistory);
      this.medicalRecordInstance.createFamilyHistory(req.body.medicalRecordID, req.body.familyHistory);
      res.status(201).json({
        message: 'Family history created successfully',
      });
    }catch (error) {
      console.error('Error creating family history:', error.message);
      res.status(500).json({
        message: 'Error creating family history',
        details: error.message
      });
    }
  }

  public async getAllMedicalRecordConditions(req: any, res: any): Promise<void> {
    try {
      const medicalConditionDTOList = await this.medicalRecordInstance.getAllMedicalRecordConditions();
      console.log('DTO CONTROLLER',medicalConditionDTOList);

      res.status(200).json({
        medicalRecordConditions: medicalConditionDTOList
      });
    } catch (error) {
      console.error('Error getting medical record conditions:', error.message);
      res.status(500).json({
        message: 'Error getting medical record conditions',
        details: error.message
      });
    }
  }


  public async updateMedicalRecordConditions(req: any, res: any): Promise<void> {
    const medicalRecordId = req.body.medicalRecordID;
    const updatedConditions = req.body.recordConditions;
    console.log("Updating medical conditions for medical record with ID CONTROLLER:", medicalRecordId);
    console.log("Updated conditions:", updatedConditions);

    try {
      await this.medicalRecordInstance.updateMedicalConditions(medicalRecordId, updatedConditions);
      res.status(200).json({
        message: 'Medical conditions updated successfully',
      });
    } catch (error: any) {
      console.error('Error updating medical conditions:', {
        message: error.message,
        stack: error.stack,
      });

      res.status(500).json({
        message: 'Error updating medical conditions',
        details: error.message,
      });
    }
  }

}

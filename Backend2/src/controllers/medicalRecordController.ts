import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import {
  MedicalConditionNotFoundException
} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {
  MedicalRecordConditionNotFoundException
} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {error} from "winston";
import {AppError} from "../domain/Shared/Exceptions/AppError";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

  constructor(
    @Inject("MedicalRecordService") private medicalRecordInstance: IMedicalRecordService,
  ) {}
  
  public async getMedicalRecordFamilyHistoryWithIds(req: any, res: any): Promise<void> {
    try {
      const medicalRecordId = req.params.id;
      const medicalRecordFamilyHistory = await this.medicalRecordInstance.getMedicalRecordFamilyHistoryWithIds(medicalRecordId);
      res.status(200).json({
        medicalRecordFamilyHistory: medicalRecordFamilyHistory
      });
    } catch (error) {
      console.error('Error getting medical record family history:', error.message);
      res.status(500).json({
        message: 'Error getting medical record family history',
        details: error.message
      });
    }
  }

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

      if (error instanceof NoMedicalRecordException) {
        res.status(500).json({
          message: error.message
        });
        return;
      }
      
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
      console.log("Family history:", req.body.familyMember);
      console.log("Family history:", req.body.condition);
      console.log("Family history:", req.body.payload);
      this.medicalRecordInstance.createFamilyHistory(req.body.medicalRecordID, req.body);
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

  public async updateMedicalRecordConditionComment(req: any, res: any): Promise<void> {
    console.log(req.body);
    const medicalRecordId = req.body.payload.id;
    const updatedComment = req.body.payload.comment;
   // console.log("Updating medical condition description for medical record with ID CONTROLLER:", medicalRecordId);
    console.log("Condition code:", medicalRecordId);
    console.log("Updated description:", updatedComment);

    try {
      await this.medicalRecordInstance.updateMedicalRecordConditionComment(medicalRecordId, updatedComment);
      res.status(200).json({
        message: 'Medical condition description updated successfully',
      });
    } catch (error: any) {
      console.error('Error updating medical condition description:', {
        message: error.message,
        stack: error.stack,
      });

      res.status(500).json({
        message: 'Error updating medical condition description',
        details: error.message,
      });
    }
  }

}
